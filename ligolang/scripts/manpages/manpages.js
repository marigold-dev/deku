const sanitize = doc => doc.replace(/</g, '&lt;').replace(/>/g, '&gt;')

const { JSDOM } = require("jsdom");
const fs = require("fs");

const html = process.argv[2]
const dom = new JSDOM(html);

const document = dom.window.document;

const lists = Array.from(document.body.querySelectorAll("ul"))
    .slice(3);

const capitalizeFirstChar = s => `${s.charAt(0).toUpperCase()}${s.slice(1)}`

const parseDoc = doc => capitalizeFirstChar(doc.split(":").slice(1)[0].trim())

const parseCommand = li => {
    const cmdlinenode = li.querySelector(".cmdline")
    const kwds = Array.from(cmdlinenode.querySelectorAll(".kwd")).map(k => k.textContent)
    const ags = Array.from(cmdlinenode.querySelectorAll(".arg")).map(a => a.textContent)
    const ops = Array.from(cmdlinenode.querySelectorAll(".opt")).map(o => o.textContent)
    const cmdline = { kwds, ags, ops }

    const cmddocnode = li.querySelector(".cmddoc")
    const cmddoc = cmddocnode ? cmddocnode.textContent.split("\n") : []
    let idx = 1
    const args = cmddocnode ? Array.from(cmddocnode.querySelectorAll(".arg"))
        .map(arg => ({ arg: arg.textContent, doc: parseDoc(cmddoc[idx++])}))
        : []
    const opts = cmddocnode ? Array.from(cmddocnode.querySelectorAll(".opt"))
        .map(opt => ({ opt: opt.textContent, doc: parseDoc(cmddoc[idx++])}))
        : []

    const entry = {
        cmdline,
        cmddoc: cmddoc[0] || "",
        args,
        opts
    }

    return entry;
}

const commands = lists.map(ul => {
    const lis = Array.from(ul.querySelectorAll("li"))
    const subcommands = lis.map(li => parseCommand(li))
    return subcommands;
}).flatMap(x => x);

const render = ({ cmdline: { kwds, ags }, cmddoc, args, opts }) => {
const kwdsContent = `**ligo ${kwds.join(" ")}**`
const agsContent = ags.map(a => `*${a}*`).join(" ")
const description = cmddoc ? `
### DESCRIPTION

${cmddoc}
` : ""
const cmdlineContent = `${kwdsContent} ${agsContent} \\[*OPTION*\\]\\...`
const argsContent = args.map(({ arg, doc }) => `**${arg}**\n\n${doc}`).join("\n\n");
const arguments = argsContent.length > 0 ? 
`### ARGUMENTS

${argsContent}
` : ""
const optsContent = opts.map(({ opt, doc }) => `**${opt}**\n\n${doc}`).join("\n\n");
const options = optsContent.length > 0 ? 
`### OPTIONS

${optsContent}
` : ""

return `
### SYNOPSIS

${cmdlineContent}
${description}
${arguments}
${options}
`
}

const basepath = "../../gitlab-pages/docs/manpages";

commands.forEach(command => {
    const name = command.cmdline.kwds.join(" ")
    fs.writeFileSync(`${basepath}/${name}.md`, sanitize(render(command)))
})

const headings = Array.from(document.body.querySelectorAll("h3"))
    .slice(0, 4).map(h => h.textContent).filter(h => h !== "Global options (must come before the command)")
const mainCommands =  Array.from(document.body.querySelectorAll("ul"))
    .slice(0, 3)

const cs = mainCommands.map(ul => {
    const lis = Array.from(ul.querySelectorAll("li"))
    const subcommands = lis.map(li => parseCommand(li))
    return subcommands;
});

const ligo = cs.map((c, i) => {
const heading = headings[i]
const commands = c.map(({ cmdline, cmddoc, args, opts }) => {
const kwds = cmdline.kwds.length > 0 ? `*${cmdline.kwds.join("")}*` : ""
const ops = cmdline.ops
    .filter(o => o !== "global options")
    .map(o => `\\[*${o.substring(0,2) === "--" ? "\\"+o : o}*\\]`)
    .join(" ")
const ags = cmdline.ags.map(a => `*${a}*`).join(" ")
const description = cmddoc ? `
#### DESCRIPTION

${cmddoc}
` : ""
const argsContent = args.map(({ arg, doc }) => `**${arg}**\n\n${doc}`).join("\n\n");
const arguments = argsContent.length > 0 ? `
#### ARGUMENTS

${argsContent}
` : ""
        
const optsContent = opts.map(({ opt, doc }) => `**${opt}**\n\n${doc}`).join("\n\n");
const options = optsContent.length > 0 ? `
#### OPTIONS

${optsContent}
` : ""

return `
**ligo** ${kwds} ${ags} ${ops}

${description}
${arguments}
${options}
`}).join("\n")
return `
### ${heading}
${commands}
`
}).join("\n\n")

fs.writeFileSync(`${basepath}/ligo.md`, sanitize(ligo))
