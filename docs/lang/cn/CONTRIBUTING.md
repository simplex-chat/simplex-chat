---
title: 贡献指南
revision: 31.01.2023
---

| 更新于 31.01.2023 | 语言: EN, [FR](/docs/lang/fr/CONTRIBUTING.md), [CZ](/docs/lang/cs/CONTRIBUTING.md), [PL](/docs/lang/pl/CONTRIBUTING.md) |

# 贡献指南

## 启用 SQLCipher 加密的编译

在项目根目录下添加 `cabal.project.local` 文件，包含 OpenSSL 头文件和库的位置以及设置加密模式的标签：

```
cp scripts/cabal.project.local.mac cabal.project.local
# 或者
# cp scripts/cabal.project.local.linux cabal.project.local
```

## MacOS 上的 OpenSSL

MacOS 默认使用 LibreSSL，必须安装 OpenSSL 才能从源代码编译 SimpleX。

可以使用 `brew install openssl@3.0` 安装 OpenSSL

你需要将 `/opt/homebrew/opt/openssl@3.0/bin` 添加到你的 PATH 中，以确保一切正常工作


## 项目分支

**在 simplex-chat 仓库中**

- `stable` - 应用程序的稳定版本，可用于更新到以前的稳定版本 (GHC 9.6.3)。

- `stable-android` - 用于使用 Nix 构建稳定的 Android 核心库 (GHC 8.10.7) - 仅适用于 Android armv7a。

- `master` - 测试版发布的分支（兼容 GHC 9.6.3 和 8.10.7）。

- `master-android` - 用于使用 Nix 构建测试版 Android 核心库 (GHC 8.10.7) - 仅适用于 Android armv7a。

**在 simplexmq 仓库中**

- `master` - 兼容 GHC 9.6.3 和 8.10.7。

## 开发和发布流程

1. 仅对 simplex-chat 和 simplexmq 仓库的 `master` 分支进行 PR。

2. 要构建 Android、iOS 和 Windows 的核心库：
- 将 `master` 分支合并到 `master-android` 分支。
- 推送到 GitHub。

3. 所有库都应从 `master` 分支构建，Android armv7a - 从 `master-android` 分支构建。

4. 要构建桌面和 CLI 应用程序，请在 `master` 分支中创建标签，APK 文件应附加到发布中。

5. 在 App Store 和 Play Store 公开发布后，合并：
- `master` 到 `stable`
- `master` 到 `master-android`（并编译/更新代码）
- `master-android` 到 `stable-android`

6. 独立地，simplexmq 仓库的 `master` 分支应在稳定版本中合并到 `stable` 分支。

## 分支和 PR

在 PR 名称中使用更改范围（或逗号分隔的范围）作为第一个词，后跟冒号。提交名称本身应为小写，使用现在时。

simplex-chat 仓库中的 PR 名称用于发布说明，它们应描述解决的问题而不是更改。可能的 PR 范围：
- ios
- android
- desktop
- core
- docs
- website
- ci

我们会压缩 PR，请勿在审核后重写分支历史记录。

对于一些复杂的功能，我们会创建功能分支，准备好后再合并 - 请勿直接提交到它们，请对功能分支进行 PR。

## GHC 8.10.7 和 GHC 9.6.3 之间的差异

1. 主要区别与 `DuplicateRecordFields` 扩展有关。

在 GHC 9.6.3 中不再可能在使用选择器时指定类型，而是使用 OverloadedRecordDot 扩展和语法，这些需要在 GHC 8.10.7 中删除：

```haskell
{-# LANGUAGE DuplicateRecordFields #-}
-- 在需要时在 GHC 9.6.3 中使用
{-# LANGUAGE OverloadedRecordDot #-}

-- GHC 9.6.3 语法
let x = record.field

-- GHC 8.10.7 语法在 GHC 9.6.3 中删除
let x = field (record :: Record)
```

在使用记录更新语法时仍然可以指定类型，使用此编译指示来抑制编译器警告：

```haskell
-- 在需要时在 GHC 9.6.3 中使用
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}

let r' = (record :: Record) {field = value}
```

2. 大多数 monad 函数现在必须从 `Control.Monad` 导入，而不是从特定的 monad 模块（例如 `Control.Monad.Except`）导入。

```haskell
-- 在需要时在 GHC 9.6.3 中使用
import Control.Monad
```

[此 PR](https://github.com/simplex-chat/simplex-chat/pull/2975/files) 包含所有差异。
