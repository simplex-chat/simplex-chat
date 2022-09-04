// Convert relative blog extension (from .md to .html)
document.querySelectorAll("a").forEach((a) => {
    const dotSepList = a.getAttribute("href").split(".");
    if (dotSepList[0] == "") {
        const hashSepList = dotSepList[dotSepList.length - 1].split("#")
        if (hashSepList[0] == "md") {
            let str = `.${dotSepList[1]}`;
            for (let i = 2; i < dotSepList.length; i++) {
                if (dotSepList[i].substring(0, 2) != "md") {
                    str += "." + dotSepList[i];
                } else {
                    str += ".html";
                    break;
                };
            }
            if (hashSepList[1]) {
                str += "#" + hashSepList[1];
            }
            a.setAttribute("href", str);
        }
    }
});