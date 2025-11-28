---
title: Installing SimpleX Chat Desktop from Ubuntu repository
revision: 28.11.2025
---

# Installing SimpleX Chat Desktop from Ubuntu repository

1. Install necessary dependencies:

    ```sh
    sudo apt update && sudo apt install -y curl gpg apt-transport-https
    ```

2. Setup SimpleX repository key:

    ```sh
    curl -sSL https://app.simplex.chat/deb-repo.gpg | gpg --dearmor | sudo tee /usr/share/keyrings/simplex-archive-keyring.gpg > /dev/null
    ```

3. Setup SimpleX repository:

    ```sh
    . /etc/os-release; printf "deb [signed-by=/usr/share/keyrings/simplex-archive-keyring.gpg] https://app.simplex.chat/deb ${VERSION_CODENAME} main" | sudo tee /etc/apt/sources.list.d/simplex.list
    ```

4. Install SimpleX Desktop:

    ```sh
    sudo apt update && sudo apt install simplex
    ```

