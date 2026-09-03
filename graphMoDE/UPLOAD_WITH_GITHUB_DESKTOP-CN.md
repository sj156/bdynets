# 使用 GitHub Desktop 上传

不要使用本机旧目录 `~/bdynets` 提交：那个副本落后于网页端且混有
许多未提交文件。也不要把江老师的仓库加成 `bdynets-Codex` 的 remote。

1. 打开 GitHub Desktop，选择 **File → Clone Repository**。
2. 切到 **URL**，输入 `https://github.com/sj156/bdynets.git`。
3. 在 **Local Path** 选择一个全新的空目录，例如
   `~/Documents/GitHub/bdynets-jiang-clean`，然后点 **Clone**。
4. 点击 **Fetch origin**，确认当前分支是 `main`。
5. 选择 **Current Branch → New Branch**，新分支命名为
   `graphmode-debug-handoff-2026-09-03`。
6. 在 Finder 中打开这个交接包的 `graphMoDE` 文件夹；把它里面的全部内容
   复制到新克隆仓库的 `graphMoDE/`。覆盖网页端原来那个空白的小写
   `readme.md`。不要复制外层 ZIP，也不要复制任何 `.git` 目录。
7. 回到 GitHub Desktop 的 **Changes**。所有变化都必须位于
   `graphMoDE/`；如果出现该目录以外的文件，先停止，不要提交。
8. 确认列表里没有大型结果 ZIP、`chains/`、`checkpoints/`、原始北京路网、
   `.Rhistory` 或 `.DS_Store`。
9. Summary 填 `Add graphMoDE simulation debug handoff`，提交到刚建的分支。
10. 点击 **Push origin**，然后点击 **Preview Pull Request / Create Pull
    Request**。标题和说明可复制 `PULL_REQUEST_TEXT.md`。
11. 把 Pull Request 链接发给江老师；先不要自行合并，让江老师在分支上
    接力 Debug 或 review 后再合并。

如果第 10 步提示没有 `sj156/bdynets` 的写权限，选择 GitHub Desktop 提供的
**Fork this repository**，把同一分支推到你自己的 fork，再向
`sj156/bdynets:main` 建 Pull Request；也可以让江老师先把你的 GitHub 账号加为
collaborator。
