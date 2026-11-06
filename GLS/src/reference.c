#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include <float.h>
#include <math.h>

/* The score is monotone, although its positive exponential scaling need not
   be. Newton uses the scaled *unscaled-score derivative*, retaining the exact
   Newton ratio without overflowing exp(eta). A maintained sign bracket and
   bisection safeguard prevent flat-likelihood or Newton stagnation. */
static double score_at(double b, double m, double R, const double *h,
                       const double *offset, const double *h2, int k,
                       long double hy, double *eta, double *derivative) {
    double scale = 0.0;
    for (int j = 0; j < k; ++j) {
        eta[j] = offset[j] + h[j] * b;
        if (!R_FINITE(eta[j])) Rf_error("Non-finite reference-filter predictor.");
        if (eta[j] > scale) scale = eta[j];
    }
    long double s = (((long double)b - m) / R - hy) * exp(-scale);
    long double d = (1.0L / R) * exp(-scale);
    for (int j = 0; j < k; ++j) {
        double e = exp(eta[j] - scale);
        s += (long double)h[j] * e;
        d += (long double)h2[j] * e;
    }
    *derivative = (double)d;
    return (double)s;
}

static void reference_one(const double *y, const double *h, const double *offset,
                          int k, double discount, double *m, double *C,
                          double *h2, double *eta) {
    double R = *C / discount;
    if (!R_FINITE(R) || R <= 0) Rf_error("Non-finite reference-filter variance.");
    long double hy = 0;
    for (int j = 0; j < k; ++j) {
        if (!R_FINITE(y[j]) || !R_FINITE(h[j]) || !R_FINITE(offset[j]))
            Rf_error("Reference inputs must be finite.");
        hy += (long double)(h[j] * y[j]);
        h2[j] = h[j] * h[j];
    }
    double width = fmax(1.0, sqrt(R)), lower = *m - width, upper = *m + width;
    double derivative, fl = 0, fu = 0;
    int bracketed = 0;
    for (int i = 0; i <= 100; ++i) {
        fl = score_at(lower, *m, R, h, offset, h2, k, hy, eta, &derivative);
        fu = score_at(upper, *m, R, h, offset, h2, k, hy, eta, &derivative);
        if (fl <= 0 && fu >= 0) { bracketed = 1; break; }
        width *= 2;
        lower = *m - width; upper = *m + width;
    }
    if (!bracketed) Rf_error("Could not bracket reference-filter mode.");
    double b = *m;
    int converged = 0;
    if (fl == 0) { b = lower; converged = 1; }
    if (fu == 0) { b = upper; converged = 1; }
    for (int i = 0; i < 1024 && !converged; ++i) {
        double f = score_at(b, *m, R, h, offset, h2, k, hy, eta, &derivative);
        if (f == 0) { converged = 1; break; }
        if (f < 0) lower = b; else upper = b;
        double step = f / derivative;
        double tolerance = 2e-12 + 4 * DBL_EPSILON * fabs(b);
        if (R_FINITE(step) && fabs(step) <= tolerance) {
            b -= step; converged = 1; break;
        }
        double next = b - step;
        if (!R_FINITE(next) || next <= lower || next >= upper ||
            fabs(step) > .75 * (upper - lower) || i % 4 == 3)
            next = lower + .5 * (upper - lower);
        if (next == b || upper - lower <= tolerance) {
            b = next; converged = 1; break;
        }
        b = next;
    }
    if (!converged) Rf_error("Reference-filter mode iteration did not converge.");
    long double precision = 1.0L / R;
    for (int j = 0; j < k; ++j)
        precision += (long double)(h2[j] * exp(offset[j] + h[j] * b));
    if (!R_FINITE((double)precision) || precision <= 0)
        Rf_error("Non-finite reference-filter precision.");
    *m = b; *C = 1 / (double)precision;
}

SEXP gls_reference_path(SEXP Y, SEXP H, SEXP offset, SEXP discount,
                        SEXP state, SEXP keep_mean) {
    SEXP dims = Rf_getAttrib(Y, R_DimSymbol);
    if (Rf_length(dims) != 2 || (!Rf_isReal(Y) && !Rf_isInteger(Y)))
        Rf_error("Y must be a numeric matrix.");
    int TT = INTEGER(dims)[0], k = INTEGER(dims)[1];
    SEXP hd = Rf_getAttrib(H, R_DimSymbol), od = Rf_getAttrib(offset, R_DimSymbol);
    if (TT < 1 || k < 1 || !Rf_isReal(H) || !Rf_isReal(offset) ||
        Rf_length(hd) < 2 || Rf_length(od) != 2 ||
        INTEGER(hd)[0] != TT || INTEGER(hd)[1] != k ||
        INTEGER(od)[0] != TT || INTEGER(od)[1] != k ||
        XLENGTH(H) != (R_xlen_t)TT * k || XLENGTH(offset) != (R_xlen_t)TT * k)
        Rf_error("Reference matrices must have matching dimensions and rank one.");
    if (!Rf_isReal(state) || Rf_length(state) != 2)
        Rf_error("Invalid initial reference state.");
    double d = Rf_asReal(discount), m = REAL(state)[0], C = REAL(state)[1];
    if (!R_FINITE(d) || d <= 0 || d >= 1 || !R_FINITE(m) || !R_FINITE(C) || C <= 0)
        Rf_error("Invalid reference discount or initial state.");
    int keep = Rf_asLogical(keep_mean);
    if (keep == NA_LOGICAL) Rf_error("keep_mean must be TRUE or FALSE.");
    SEXP ans = PROTECT(Rf_allocVector(VECSXP, keep ? 4 : 3));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, keep ? 4 : 3));
    SET_STRING_ELT(names, 0, Rf_mkChar("W"));
    SET_STRING_ELT(names, 1, Rf_mkChar("m"));
    SET_STRING_ELT(names, 2, Rf_mkChar("C"));
    SET_VECTOR_ELT(ans, 0, Rf_allocVector(REALSXP, TT));
    if (keep) {
        SET_STRING_ELT(names, 3, Rf_mkChar("mean_path"));
        SET_VECTOR_ELT(ans, 3, Rf_allocVector(REALSXP, TT));
    }
    double *y = (double *) R_alloc(k, sizeof(double));
    double *h = (double *) R_alloc(k, sizeof(double));
    double *o = (double *) R_alloc(k, sizeof(double));
    double *h2 = (double *) R_alloc(k, sizeof(double));
    double *eta = (double *) R_alloc(k, sizeof(double));
    for (int t = 0; t < TT; ++t) {
        if (t % 32 == 0) R_CheckUserInterrupt();
        REAL(VECTOR_ELT(ans, 0))[t] = (1 / d - 1) * C;
        for (int j = 0; j < k; ++j) {
            R_xlen_t pos = t + (R_xlen_t)TT * j;
            y[j] = Rf_isReal(Y) ? REAL(Y)[pos] : INTEGER(Y)[pos];
            h[j] = REAL(H)[pos]; o[j] = REAL(offset)[pos];
        }
        reference_one(y, h, o, k, d, &m, &C, h2, eta);
        if (keep) REAL(VECTOR_ELT(ans, 3))[t] = m;
    }
    SET_VECTOR_ELT(ans, 1, Rf_ScalarReal(m));
    SET_VECTOR_ELT(ans, 2, Rf_ScalarReal(C));
    Rf_setAttrib(ans, R_NamesSymbol, names);
    UNPROTECT(2);
    return ans;
}

static const R_CallMethodDef CallEntries[] = {
    {"gls_reference_path", (DL_FUNC) &gls_reference_path, 6},
    {NULL, NULL, 0}
};

void attribute_visible R_init_GLS(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
