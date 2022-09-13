(async function () {
  let DELAY = 0;
  const DISTR = 0.33;

  class User {
    constructor(name) {
      this.userWindow = document.querySelector(`#demo .user.${name}`);
      this.terminal = this.userWindow.querySelector(`.terminal`);
      this.input = this.terminal.querySelector(".input");
      this.demoInput = this.terminal.querySelector("input");
      this.resetInput();
      this.setupDemo();
      this.group = [];
      this.display = this.terminal.querySelector(".display");
      this.setupMoveWindow();
      this.name = name;
    }

    reset() {
      this.resetInput();
      this.display.innerHTML = "";
    }

    setGroup(groupName, users) {
      this.users = users;
      this.group = users.filter((u) => u !== this);
      this.groupName = groupName;
    }

    tryDemo() {
      this.reset();
      show(this.demoInput);
      this.demoInput.value = "";
    }

    async send(to, message, typeTo, paste, secret) {
      await this._sendMsg(`@${to.name}`, message, typeTo, paste, secret);
      await to.receive(this, toSecret(secret, message));
      await delay(20);
    }

    async sendGroup(message, typeTo, paste) {
      await this._sendMsg(`#${this.groupName}`, message, typeTo, paste);
      await this.receiveGroup(message);
      await delay(10);
    }

    async _sendMsg(toStr, message, typeTo, paste, secret) {
      await this.type(`${toStr} `, !typeTo);
      if (secret) await this.type("#");
      await this.type(message, paste);
      if (secret) await this.type("#");
      await delay(10);
      this.resetInput();
      this.show("sent", `${toStr} ${toSecret(secret, message)}`);
    }

    async type(str, paste) {
      if (paste) {
        await delay(10);
        this.input.insertAdjacentHTML("beforeend", str);
      } else {
        for (const char of str) {
          await delay(isAlpha(char) ? 1 : 2);
          this.input.insertAdjacentHTML("beforeend", char);
        }
      }
      await delay(2);
    }

    resetInput() {
      this.input.innerHTML = "> ";
      show(this.demoInput, false);
    }

    async receive(from, message, edit, group) {
      await delay(10);
      let g = group ? `#${this.groupName} ` : "";
      this.show("received", `${g}${from.name}&gt; ${message}`, edit);
    }

    async receiveGroup(message, edit) {
      await Promise.all(this.group.map((u) => u.receive(this, message, edit, true)));
    }

    show(mode, str, edit) {
      if (edit && this.lastMessage) {
        this.lastMessage.innerHTML = highlight(str);
        return;
      }
      this.display.insertAdjacentHTML("beforeend", `<div class="${mode}">${highlight(str)}</div>`);
    }

    setupDemo() {
      if (!this.demoInput) return;
      let editMode = false;

      on("keypress", this.demoInput, async ({ key }) => {
        if (key === "Enter") {
          const edit = editMode;
          editMode = false;
          const [to, ...words] = this.demoInput.value.trim().split(" ");
          const message = words.join(" ");
          switch (to[0]) {
            case undefined:
              if (message !== "") {
                this.show("error", "Message should start with @user or #group");
              }
              break;
            case "@":
              await this.sendInput(to.slice(1), message, edit);
              break;
            case "#":
              await this.sendInputGroup(to.slice(1), message, edit);
              break;
            default:
              this.show("error", "Message should start with @user or #group");
          }
        } else if (this.demoInput.value === "" && key !== "@" && key !== "#") {
          const channel = this.currentChannel();
          if (channel) this.demoInput.value = channel + " ";
        }
      });
      on("keydown", this.demoInput, async (e) => {
        switch (e.key) {
          case "ArrowUp":
            if (this.demoInput.value === "" && this.lastMessage) {
              const str = (this.demoInput.value = this.lastMessage.innerText);
              editMode = true;
              await delay(0);
              this.demoInput.selectionStart = str.length;
            }
            break;
          case "Tab": {
            e.preventDefault();
            const userIndex = this.users.indexOf(this);
            const nextIndex = (userIndex + 1) % this.users.length;
            this.users[nextIndex].demoInput.focus();
          }
        }
      });
    }

    async sendInput(name, message, edit) {
      if (name === this.name) {
        this.show("error", "Can't send message to yourself");
        return;
      }
      const recipient = this.group.find((u) => u.name === name);
      if (recipient === undefined) {
        const knownNames = this.group.map((u) => `@${u.name}`).join(", ") + ` or @${this.name}`;
        this.show("error", `Unknown recipient @${name} (try ${knownNames})`);
        return;
      }
      this.show("sent", `@${name} ${message}`, edit);
      this.demoInput.value = "";
      await recipient.receive(this, message, edit);
    }

    async sendInputGroup(name, message, edit) {
      if (name !== this.groupName) {
        this.show("error", `Unknown group #${name} (try #team)`);
        return;
      }
      this.show("sent", `#${name} ${message}`, edit);
      this.demoInput.value = "";
      await this.receiveGroup(message, edit);
    }

    get lastMessage() {
      const messages = this.display.childNodes;
      return messages[messages.length - 1];
    }

    currentChannel() {
      return this.lastMessage && toContact(this.lastMessage.childNodes[0].innerHTML);
    }

    setupMoveWindow() {
      let moving = false;
      let startX, startY;
      const user = this.userWindow;
      const parent = user.parentNode;

      on("mousedown", this.terminal, (e) => {
        if (e.clientY - this.terminal.getBoundingClientRect().top > 20) return;
        moving = true;
        startX = user.offsetLeft - e.clientX;
        startY = user.offsetTop - e.clientY;
        parent.removeChild(user);
        parent.appendChild(user);
      });
      on("mouseup", this.terminal, () => (moving = false));
      on("mouseleave", this.terminal, () => (moving = false));
      on("mousemove", this.terminal, (e) => {
        if (!moving) return;
        user.style.left = e.clientX + startX + "px";
        user.style.top = e.clientY + startY + "px";
      });
    }
  }

  function toContact(str) {
    return str.endsWith("&gt;") ? "@" + str.slice(0, -4) : str;
  }

  function setGroup(groupName, users) {
    users.forEach((u) => u.setGroup(groupName, users));
  }

  const alice = new User("alice");
  const bob = new User("bob");
  const tom = new User("tom");
  const team = [alice, bob, tom];
  setGroup("team", team);

  async function chatDemo() {
    team.forEach((u) => u.reset());
    await alice.sendGroup("please review my PR project/site#72", true);
    await tom.sendGroup("anybody got application key 🔑?");
    await bob.sendGroup("looking at it now @alice 👀");
    await alice.sendGroup("thanks @bob!");
    await alice.sendGroup("will DM @tom");
    await alice.send(tom, "w3@o6CewoZx#%$SQETXbWnus", true, true, true);
    await tom.send(alice, "you're the savior 🙏!");
    await alice.send(bob, "please check the tests too", true);
    await bob.send(alice, "all looks good 👍");
    await alice.send(bob, "thank you!");
    DELAY = 80;
  }

  const invitation =
    "smp::example.com:5223#1XNE1m2E1m0lm92&#8203;WG&#8203;Ket9CL6+lO742Vy5&#8203;G6nsrkvgs8=::St9hPY+k6nfrbaXj::rsa:MII&#8203;BoTANBgkqhkiG9w0B&#8203;AQEFAAOCAY4AMIIBiQKCAQEA03XGpEqh3faDN&#8203;Gl06pPhaT==";

  async function establishConnection() {
    team.forEach((u) => u.reset());
    await alice.type("/add bob");
    await delay(10);
    alice.resetInput();
    // alice.show("/add bob");
    alice.show("sent", "pass this invitation to your contact (via any channel):");
    alice.show("sent", "&nbsp;");
    alice.show("sent", invitation);
    alice.show("sent", "&nbsp;");
    alice.show("sent", "and ask them to connect:");
    alice.show("sent", "/c name_for_you invitation_above");
    await delay(20);
    await bob.type("/connect alice ");
    await bob.type(invitation, true);
    await delay(20);
    bob.resetInput();
    await bob.show("received", "/connect alice " + invitation);
    await delay(10);
    bob.show("received", "@alice connected");
    await delay(2);
    alice.show("received", "@bob connected");
    await alice.send(bob, "hello bob");
    await bob.send(alice, "hi alice");
  }

  await chatDemo();
  const RUN_DEMO = "#demo .run-demo";
  const RUN_FASTER = "#demo .run-faster";
  const TRY_IT = "#demo .try-it";
  onClick(RUN_DEMO, runChatDemo);
  // onClick(RUN_DEMO, establishConnection);
  onClick(RUN_FASTER, () => (DELAY /= 2));
  onClick(TRY_IT, tryChatDemo);

  async function runChatDemo() {
    document.querySelectorAll('.all-users div.user').forEach(e => e.classList.remove('d-none'));
    document.querySelector('.all-users div.simplex_mobile').classList.add('d-none');
    show(RUN_DEMO, false);
    show(RUN_FASTER);
    enable(TRY_IT, false);
    await chatDemo();
    show(RUN_DEMO);
    show(RUN_FASTER, false);
    enable(TRY_IT);
  }

  function tryChatDemo() {
    document.querySelectorAll('.all-users div.user').forEach(e => e.classList.remove('d-none'));
    document.querySelector('.all-users div.simplex_mobile').classList.add('d-none');
    team.forEach((u) => u.tryDemo());
    alice.demoInput.focus();
  }

  async function delay(units) {
    // delay is random with `1 +/- DISTR` range
    const ms = units * DELAY * (1 - DISTR + 2 * DISTR * Math.random());
    return new Promise((resolve) => setTimeout(resolve, ms));
  }

  function highlight(str) {
    return str
      .replace(/(@[a-z]+)([^0-9]|$)/gi, `<span class="recipient">$1</span>$2`)
      .replace(/([a-z]+&gt;)([^0-9]|$)/gi, `<span class="sender">$1</span>$2`)
      .replace(/(#[a-z]+)([^0-9]|$)/gi, `<span class="group">$1</span>$2`)
      .replace(/#([^\s]+)#([\s]|$)/gi, `#<span class="secret">$1</span>#$2`);
  }

  function toSecret(secret, message) {
    return secret ? `#${message}#` : message;
  }

  function isAlpha(c) {
    c = c.toUpperCase();
    return c >= "A" && c <= "Z";
  }

  let flipper = setInterval(flipProblem, 10000);

  onClick("#problem .pagination", () => {
    clearInterval(flipper);
    flipper = setInterval(flipProblem, 20000);
  });

  function flipProblem() {
    if (isElementInViewport(document.getElementById("problem"))) {
      window.location.hash =
        window.location.hash === "#problem-explained" ? "#problem-intro" : "#problem-explained";
    }
  }

  function isElementInViewport(el) {
    const r = el.getBoundingClientRect();
    return r.bottom >= 0 && r.top <= window.innerHeight;
  }

  function onClick(selector, handler, enable = true) {
    const el = document.querySelector(selector);
    if (el) on("click", el, handler, enable);
  }

  function on(event, el, handler, enable = true) {
    const method = enable ? "addEventListener" : "removeEventListener";
    el[method](event, handler);
  }

  function show(selector, visible = true) {
    const el = typeof selector === "string" ? document.querySelector(selector) : selector;
    if (el) el.style.display = visible ? "block" : "none";
  }

  function enable(selector, enabled = true) {
    const el = document.querySelector(selector);
    el.disabled = enabled ? "" : "true";
  }

  const copyEls = document.querySelectorAll(".content_copy_with_tooltip");
  if (navigator.clipboard) {
    copyEls.forEach(contentCopyWithTooltip)
  } else {
    copyEls.forEach(el => el.style.visibility = "hidden")
  }

  function contentCopyWithTooltip(parent) {
    const content = parent.querySelector(".content");
    const tooltip = parent.querySelector(".tooltiptext");
    const copyButton = parent.querySelector(".content_copy");
    copyButton.addEventListener("click", copyAddress)
    copyButton.addEventListener("mouseout", resetTooltip)

    function copyAddress() {
      navigator.clipboard.writeText(content.innerText || content.value);
      tooltip.innerHTML = "Copied!";
    }

    function resetTooltip() {
      tooltip.innerHTML = "Copy to clipboard";
    }
  }

  // Setting width for scrollable sections
  const element = document.querySelector('.container');
  const computedStyle = getComputedStyle(element);
  const screenWidth = element.clientWidth - (parseFloat(computedStyle.paddingLeft) + parseFloat(computedStyle.paddingRight));
  document.querySelector('.table-holder').style.width = `${screenWidth}px`;
  Array.from(document.querySelectorAll('.problem-section')).forEach(ele => ele.style.width = `${screenWidth}px`);

})();
