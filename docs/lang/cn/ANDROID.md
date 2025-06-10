---
title: 访问Android应用程序中的文件
revision: 07.02.2023
---

| 07.02.2023 | ZH_CN, EN, [CZ](/docs/lang/cs/ANDROID.md), [FR](/docs/lang/fr/ANDROID.md), [波兰文](/docs/lang/pl/ANDROID.md) |

# 访问Android应用程序中的文件

SimpleX使用数据库并将其首选项存储在Android的私有数据目录中。目录包括：
- 数据库
- 发送和接收的文件
- 空闲时会被删除的临时文件
- 用户偏好

如果你想要查看SimpleX私有数据目录中存储的内容，你需要有：
- 类Unix操作系统 (Windows用户 [MinGW](https://www.mingw-w64.org/downloads/))
- 一台已安装 ADB (Android Debug Bridge) 工具的电脑([下载链接](https://developer.android.com/studio/releases/platform-tools))
- 你的设备通过USB或者Wi-Fi连接到电脑

## 操作步骤：

- 打开 SimpleX，进入 `数据库密码和导出` ，启用 `应用数据备份` 。这将确保其他步骤正常进行
- _可选_： 如果要查看数据库内容，请将数据库密码从随机值更改为你的密码。为此，请在 `数据库密码和导出` 中停止聊天（关闭 `聊天运行中` ），打开 `数据库密码` ，输入新的密码并确认，然后更新它。不要忘记密码，否则再次被问到密码短语时你将丢失所有数据
- 打开一个终端模拟器 (Windows CMD/Powershell 不会正常工作) 并更改到你将用于存储和备份的目录：

```bash
cd /tmp  # 仅供参考
```
然后执行如下命令：
```bash
adb -d backup -f chat.ab -noapk chat.simplex.app && 
tail -n +5 chat.ab > chat.dat && 
printf "\x1f\x8b\x08\x00\x00\x00\x00\x00" | cat - chat.dat > chat.gz && 
tar -xvzf chat.gz
```

现在解锁你的设备并确认无密码保护的备份操作，在解锁前这些都不会正常工作。

在此之后备份应该已结束。如果在打印一些文件名之前你看到了报错 `tar: Error is not recoverable: exiting now` ，别担心，一切顺利。

备份的文件会存放到 `./apps/chat.simplex.app/`.

请注意，如果你使用的是更新版本的 SimpleX，数据库将被加密，你将无法在不知道解密密码的情况下通过 `sqlcipher` 程序访问内容（你应该首先将随机生成的密码改成你自己的。）

## 解密数据库

在查看数据库内容前你需要解密。使用你喜欢的包管理器安装 `sqlcipher` 然后在存放数据库的目录下执行这些命令：
```bash
sqlcipher files_chat.db
pragma key="youDecryptionPassphrase";
# Ensure it works fine
select * from users;
```

如果你看见 `Parse error: no such table: users`, 确保你输入了正确的密码，以及你此前在 Android app上更改过随机密码（如果数据库来自Android设备，当然要这样做）
