---
title: 贡献指南
revision: 28.11.2025
---

| Updated 28.11.2025 | Languages: ZH-CN, [EN](/docs/CONTRIBUTING.md), [FR](/docs/lang/fr/CONTRIBUTING.md), [CZ](/docs/lang/cs/CONTRIBUTING.md), [PL](/docs/lang/pl/CONTRIBUTING.md) |

# 贡献指南

## 启用 SQLCipher 加密编译

在项目根目录添加 `cabal.project.local` 文件，文件中包含 OpenSSL 头文件和库文件的位置，以及设置加密模式的标志:

```
cp scripts/cabal.project.local.mac cabal.project.local
# or
# cp scripts/cabal.project.local.linux cabal.project.local
```

## MacOS 上的 OpenSSL

MacOS 默认使用 LibreSSL，必须安装 OpenSSL 才能从源代码编译 SimpleX。

可以通过 `brew install openssl@3.0` 来安装 OpenSSL

您需要将 `/opt/homebrew/opt/openssl@3.0/bin` 添加到 PATH 环境变量中，才能使相关功能正常运行。


## 项目分支

**simplex-chat 仓库**

- `stable` - 应用程序的稳定版本，可用于更新之前的稳定版本（GHC 9.6.3）。

- `stable-android` - 用于构建稳定的 Android 核心库，采用 Nix（GHC 8.10.7）构建环境——仅适用于Android armv7a架构。

- `master` - 用于发布 beta 版本的分支（兼容GHC 9.6.3和8.10.7）。

- `master-android` - 用于构建 beta 版本的 Android 核心库，采用 Nix（GHC 8.10.7）构建环境——仅适用于Android armv7a架构。

**simplexmq 仓库**

- `master` - 兼容 GHC 9.6.3 和 8.10.7。

## 开发与发布流程

1. _仅_ 向 `master` 分支提交针对 simplex-chat 和 simplexmq 两个仓库的 PR 。

2. To build core libraries for Android, iOS and windows:
- merge `master` branch to `master-android` branch.
- push to GitHub.

2. 为 Android、iOS 和 Windows 构建核心库：
- 将 `master` 分支合并到 `master-android` 分支。
- 推送到 GitHub。

3. 所有的库都是从 `master` 分支构建，Android armv7a 架构的库是从 `master-android` 分支构建。

4. 构建桌面和命令行应用，请在 `master` 分支上创建一个标签，APK 文件应附加到发布中。

5. 在 App Store 和 Play 商店公开发布后，合并：
- `master` 分支合并到 `stable`
- `master` 分支合并到 `master-android` (以及编译、更新代码)
- `master-android` 分支合并到 `stable-android`

6. 在稳定版本发布时，应将 simplexmq 仓库的 `master` 分支独立合并至 `stable` 分支。

## 分支和 PR

在 PR 名称中，请将变更范围（或以逗号分隔的多个范围）作为首词，后接冒号。提交名称本身应采用小写字母且使用现在时态。

simplex-chat 仓库中的 PR 名称用于发布说明，应描述已解决的问题而非变更内容。PR 可能的范围包括：
- ios
- android
- desktop
- core
- docs
- website
- ci

我们会压缩 PR，审查后禁止重写分支历史。

对于某些复杂功能，我们会创建功能分支，待准备就绪后统一合并——请勿直接向功能分支提交代码，而应通过PR方式合并到功能分支。

## GHC 8.10.7 与 GHC 9.6.3 之间的差异

1. 核心区别与 `DuplicateRecordFields` 语言扩展相关。

在 GHC 9.6.3 中，使用记录选择器时已无法再指定类型，取而代之的是采用 `OverloadedRecordDot` 扩展及相应语法——而这些在 GHC 8.10.7 版本中必须被移除：

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
-- use this in GHC 9.6.3 when needed
{-# LANGUAGE OverloadedRecordDot #-}

-- GHC 9.6.3 syntax
let x = record.field

-- GHC 8.10.7 syntax removed in GHC 9.6.3
let x = field (record :: Record)
```

在使用记录更新语法时，目前仍可指定类型。如需屏蔽编译器警告，请使用以下编译指示：

```haskell
-- 在 GHC 9.6.3 中若需使用此功能，请按需添加以下编译指示：
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

let r' = (record :: Record) {field = value}
```

2. 大多数 monad 函数现在必须从 `Control.Monad` 导入，而不再从特定的单子模块（例如 `Control.Monad.Except` ）导入。

```haskell
-- 在 GHC 9.6.3 中需要时使用
import Control.Monad
```

[此 PR](https://github.com/simplex-chat/simplex-chat/pull/2975/files) 包含所有差异。

## 增强远程桌面连接的跨版本兼容性

界面已能处理聊天及聊天项在JSON转换失败的情况，不仅便于调试，也支持版本降级的需求。

虽然我们可以通过升级远程连接版本号来实现不同版本间的互不兼容，但这会降低远程连接的用户体验——因为在许多情况下，由于移动端和桌面端应用的发版周期不同，用户往往无法同时升级两端应用。

这对Android应用用户尤其不便，他们只能通过导出/导入功能进行降级，无法直接在新版本上安装旧版应用。

PR #6105 通过以下方式改善了该问题：
- 添加 CInfoInvalidJSON 构造函数，使得无法解析的聊天将通过远程连接显示为“无效聊天”（类似于UI中缺少API中字段的情况），
- 更改 CIContent 的 JSON 解析方式，使其在平台特定的 JSON 解析器中回退到 CIInvalidJSON。

为了避免列表中的“无效”聊天，我们需要在 JSON 编码级别上维护 AChat 类型及其子类型的正向兼容性：
- 将新字段添加为可选字段，
- 在 JSON 实例中为新字段的 FromJSON 实例添加 `omittedField` 方法，以提供适当的默认值，
- 将原始非可选字段定义为带有 `omittedField` 的新类型，并在 JSON 实例中使用。

为避免聊天项回退至无效JSON状态，我们同样需要对 ChatItem 类型及其子类型采取相同措施。这一点在向所有 CIContent 使用的类型添加字段时尤为重要，否则所有消息项都将无法正常显示。
