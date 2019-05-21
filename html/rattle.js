"use strict";
function bindPlot(element, data, options) {
    const redraw = () => {
        if ($(element).is(":visible"))
            $.plot($(element), data.get(), options);
    };
    window.setTimeout(redraw, 1);
    $(window).on("resize", redraw);
    data.event(redraw);
}
function varLink(name) {
    return React.createElement("a", { href: "https://hackage.haskell.org/package/shake/docs/Development-Shake.html#v:" + name },
        React.createElement("tt", null, name));
}
function newTable(columns, data, sortColumn, sortDescend) {
    const f = (x) => ({ name: x.field, label: x.label, width: x.width, cellClasses: x.alignRight ? "right" : "" });
    const formatters = {};
    for (const c of columns)
        formatters[c.field] = c.show || ((x) => x);
    const table = new DGTable({
        adjustColumnWidthForSortArrow: false,
        cellFormatter: (val, colname) => formatters[colname](val),
        columns: columns.map(f),
        width: DGTable.Width.SCROLL
    });
    $(table.el).css("height", "100%");
    window.setTimeout(() => {
        table.render();
        table.tableHeightChanged();
        if (sortColumn)
            table.sort(sortColumn, sortDescend);
        table.setRows(data.get(), true);
    }, 1);
    let toRender = false;
    data.event(xs => {
        table.setRows(xs, true);
        if ($(table.el).is(":visible"))
            table.render();
        else
            toRender = true;
    });
    $(window).on("resize", () => {
        if ($(table.el).is(":visible")) {
            table.tableHeightChanged();
            if (toRender) {
                table.render();
                toRender = false;
            }
        }
    });
    return React.createElement("div", { style: "height:100%;width:100%;" }, table.el);
}
// These are global variables mutated/queried by query execution
let environmentAll; // All the profiles
let environmentThis; // The specific profile under test
let environmentGroup; // The group produced as a result
function group(x) {
    environmentGroup.push(x);
    return true;
}
function leaf() {
    return environmentThis.writers.length === 0;
}
function run(i) {
    if (i === undefined)
        return environmentThis.built;
    else
        return environmentThis.built === i;
}
function named(r, groupName) {
    if (r === undefined)
        return environmentThis.name;
    const res = execRegExp(r, environmentThis.name);
    if (res === null) {
        if (groupName === undefined)
            return false;
        else {
            group(groupName);
            return true;
        }
    }
    if (res.length !== 1) {
        for (let i = 1; i < res.length; i++)
            group(res[i]);
    }
    return true;
}
function profileLoaded(profileRaw) {
    $(document.body).empty().append(profileRoot(unraw(profileRaw)));
}
function unraw(xs) {
    const ans = xs.map((x, i) => ({
        index: i,
        name: x[0],
        execution: x[1],
        built: x[2],
        builtLast: x[3],
        changed: x[4],
        filesWritten: x[5],
        filesRead: x[6],
        readers: x[7],
        writers: x[8],
        hazards: x[9] // depends and rdepends that violate consistency
    }));
    return ans;
}
function profileRoot(profile) {
    const [s, search] = createSearch(profile);
    const t = createTabs([["Summary", () => reportSummary(profile)]
        // , ["Command plot", () => reportCmdPlot(profile)]
        // , ["Commands", () => reportCmdTable(profile, search)]
        ,
        ["Commands", () => reportRuleTable(profile, search)],
        ["Parallelizability", () => reportParallelism(profile)],
        ["Details", () => reportDetails(profile, search)]
        // , ["Why rebuild", () => reportRebuild(profile, search)]
    ]);
    return React.createElement("table", { class: "fill" },
        React.createElement("tr", null,
            React.createElement("td", { style: "padding-top: 8px; padding-bottom: 8px;" },
                React.createElement("a", { href: "https://github.com/ndmitchell/rattle", style: "font-size: 20px; text-decoration: none; color: #3131a7; font-weight: bold;" }, "Rattle profile report"),
                React.createElement("span", { style: "color:gray;white-space:pre;" },
                    "   - generated at ",
                    generated,
                    " by Rattle v",
                    version))),
        React.createElement("tr", null,
            React.createElement("td", null, s)),
        React.createElement("tr", null,
            React.createElement("td", { height: "100%" }, t)));
}
function createTabs(xs) {
    const bodies = xs.map(x => {
        const el = React.createElement("div", { style: "padding:5px;width:100%;height:100%;min-width:150px;min-height:150px;overflow:auto;display:none;" });
        const upd = lazy(() => $(el).append(x[1]()));
        return pair(el, upd);
    });
    let lbls = [];
    const f = (i) => () => {
        bodies[i][1]();
        lbls.map((x, j) => $(x).toggleClass("active", i === j));
        bodies.map((x, j) => $(x[0]).toggle(i === j));
        $(window).trigger("resize");
    };
    lbls = xs.map((x, i) => React.createElement("a", { onclick: f(i) }, x[0]));
    f(0)();
    return React.createElement("table", { class: "fill" },
        React.createElement("tr", null,
            React.createElement("td", null,
                React.createElement("table", { width: "100%", style: "border-spacing:0px;" },
                    React.createElement("tr", { class: "tabstrip" },
                        React.createElement("td", { width: "20", class: "bottom" }, "\u00A0"),
                        React.createElement("td", { style: "padding:0px;" }, lbls),
                        React.createElement("td", { width: "100%", class: "bottom" }, "\u00A0"))))),
        React.createElement("tr", { height: "100%" },
            React.createElement("td", { style: "background-color:white;" }, bodies.map(fst))));
}
// A mapping from names (rule names or those matched from rule parts)
// to the indicies in profiles.
class Search {
    constructor(profile, mapping) {
        this.profile = profile;
        if (mapping !== undefined)
            this.mapping = mapping;
        else {
            this.mapping = {};
            for (const p of profile)
                this.mapping[p.name] = [p.index];
        }
    }
    forEachProfiles(f) {
        for (const s in this.mapping)
            f(this.mapping[s].map(i => this.profile[i]), s);
    }
    forEachProfile(f) {
        this.forEachProfiles((ps, group) => ps.forEach(p => f(p, group)));
    }
    mapProfiles(f) {
        const res = [];
        this.forEachProfiles((ps, group) => res.push(f(ps, group)));
        return res;
    }
    mapProfile(f) {
        const res = [];
        this.forEachProfile((p, group) => res.push(f(p, group)));
        return res;
    }
}
function createSearch(profile) {
    const caption = React.createElement("div", null,
        "Found ",
        profile.length,
        " entries, not filtered or grouped.");
    const input = React.createElement("input", { id: "search", type: "text", value: "", placeholder: "Filter and group", style: "width: 100%; font-size: 16px; border-radius: 8px; padding: 5px 10px; border: 2px solid #999;" });
    const res = new Prop(new Search(profile));
    $(input).on("change keyup paste", () => {
        const s = $(input).val();
        if (s === "") {
            res.set(new Search(profile));
            $(caption).text("Found " + profile.length + " entries, not filtered or grouped.");
        }
        else if (s.indexOf("(") === -1) {
            const mapping = {};
            let found = 0;
            for (const p of profile) {
                if (p.name.indexOf(s) !== -1) {
                    found++;
                    mapping[p.name] = [p.index];
                }
            }
            res.set(new Search(profile, mapping));
            $(caption).text("Substring filtered to " + found + " / " + profile.length + " entries, not grouped.");
        }
        else {
            let f;
            try {
                f = new Function("return " + s);
            }
            catch (e) {
                $(caption).text("Error compiling function, " + e);
                return;
            }
            const mapping = {};
            let groups = 0;
            let found = 0;
            environmentAll = profile;
            for (const p of profile) {
                environmentThis = p;
                environmentGroup = [];
                let bool;
                try {
                    bool = f();
                }
                catch (e) {
                    $(caption).text("Error running function, " + e);
                    return;
                }
                if (bool) {
                    found++;
                    const name = environmentGroup.length === 0 ? p.name : environmentGroup.join(" ");
                    if (name in mapping)
                        mapping[name].push(p.index);
                    else {
                        groups++;
                        mapping[name] = [p.index];
                    }
                }
            }
            res.set(new Search(profile, mapping));
            $(caption).text("Function filtered to " + found + " / " + profile.length + " entries, " +
                (groups === found ? "not grouped." : groups + " groups."));
        }
    });
    const body = React.createElement("table", { width: "100%", style: "padding-bottom: 17px;" },
        React.createElement("tr", null,
            React.createElement("td", { width: "100%" }, input),
            React.createElement("td", { style: "padding-left:6px;padding-right: 6px;" }, searchHelp(input))),
        React.createElement("tr", null,
            React.createElement("td", null, caption)));
    return [body, res];
}
function searchHelp(input) {
    const examples = [["Only the last run", "run(0)"],
        ["Named 'Main'", "named(\"Main\")"],
        ["Group by file extension", "named(/(\\.[_0-9a-z]+)$/)"],
        ["No dependencies (an input)", "leaf()"],
        ["Didn't change when it last rebuilt", "unchanged()"],
        ["Ran 'gcc'", "command(\"gcc\")"]
    ];
    const f = (code) => () => {
        $(input).val((i, x) => x + (x === "" ? "" : " && ") + code);
        $(input).trigger("change");
    };
    const dropdown = React.createElement("div", { class: "dropdown", style: "display:none;" },
        React.createElement("ul", { style: "padding-left:30px;" }, examples.map(([desc, code]) => React.createElement("li", null,
            React.createElement("a", { onclick: f(code) },
                React.createElement("tt", null, code)),
            " ",
            React.createElement("span", { class: "note" }, desc)))));
    const arrow_down = React.createElement("span", { style: "vertical-align:middle;font-size:80%;" }, "\u25BC");
    const arrow_up = React.createElement("span", { style: "vertical-align:middle;font-size:80%;display:none;" }, "\u25B2");
    const show_inner = () => { $(dropdown).toggle(); $(arrow_up).toggle(); $(arrow_down).toggle(); };
    return React.createElement("div", null,
        React.createElement("button", { style: "white-space:nowrap;padding-top:5px;padding-bottom:5px;", onclick: show_inner },
            React.createElement("b", { style: "font-size:150%;vertical-align:middle;" }, "+"),
            "\u00A0 Filter and Group \u00A0",
            arrow_down,
            arrow_up),
        dropdown);
}
// Stuff that Shake generates and injects in
function untraced(p) {
    return 0;
}
/////////////////////////////////////////////////////////////////////
// BASIC UI TOOLKIT
class Prop {
    constructor(val) { this.val = val; this.callback = () => { return; }; }
    get() { return this.val; }
    set(val) {
        this.val = val;
        this.callback(val);
    }
    event(next) {
        const old = this.callback;
        this.callback = val => { old(val); next(val); };
        next(this.val);
    }
    map(f) {
        const res = new Prop(f(this.get()));
        this.event(a => res.set(f(a)));
        return res;
    }
}
jQuery.fn.enable = function (x) {
    // Set the values to enabled/disabled
    return this.each(function () {
        if (x)
            $(this).removeAttr("disabled");
        else
            $(this).attr("disabled", "disabled");
    });
};
/////////////////////////////////////////////////////////////////////
// BROWSER HELPER METHODS
// Given "?foo=bar&baz=1" returns {foo:"bar",baz:"1"}
function uriQueryParameters(s) {
    // From https://stackoverflow.com/questions/901115/get-querystring-values-with-jquery/3867610#3867610
    const params = {};
    const a = /\+/g; // Regex for replacing addition symbol with a space
    const r = /([^&=]+)=?([^&]*)/g;
    const d = (x) => decodeURIComponent(x.replace(a, " "));
    const q = s.substring(1);
    while (true) {
        const e = r.exec(q);
        if (!e)
            break;
        params[d(e[1])] = d(e[2]);
    }
    return params;
}
/////////////////////////////////////////////////////////////////////
// STRING FORMATTING
function showTime(x) {
    function digits(x) { const s = String(x); return s.length === 1 ? "0" + s : s; }
    if (x >= 3600) {
        x = Math.round(x / 60);
        return Math.floor(x / 60) + "h" + digits(x % 60) + "m";
    }
    else if (x >= 60) {
        x = Math.round(x);
        return Math.floor(x / 60) + "m" + digits(x % 60) + "s";
    }
    else
        return x.toFixed(2) + "s";
}
function showPerc(x) {
    return (x * 100).toFixed(2) + "%";
}
function showInt(x) {
    // From https://stackoverflow.com/questions/2901102/how-to-print-a-number-with-commas-as-thousands-separators-in-javascript
    // Show, with commas
    return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}
function showRun(run) {
    return run === 0 ? "Latest run" : run + " run" + plural(run) + " ago";
}
function showBool(b) {
    return b === 1 ? "True" : "False";
}
function plural(n, not1 = "s", is1 = "") {
    return n === 1 ? is1 : not1;
}
/////////////////////////////////////////////////////////////////////
// MISC
function compareFst(a, b) {
    return a[0] - b[0];
}
function compareSnd(a, b) {
    return a[1] - b[1];
}
function compareSndRev(a, b) {
    return b[1] - a[1];
}
function pair(a, b) {
    return [a, b];
}
function triple(a, b, c) {
    return [a, b, c];
}
function fst([x, _]) {
    return x;
}
function snd([_, x]) {
    return x;
}
function execRegExp(r, s) {
    if (typeof r === "string")
        return s.indexOf(r) === -1 ? null : [];
    else
        return r.exec(s);
}
function cache(key, op) {
    const store = {};
    return k => {
        const s = key(k);
        if (!(s in store))
            store[s] = op(k);
        return store[s];
    };
}
function lazy(thunk) {
    let store = null;
    let done = false;
    return () => {
        if (!done) {
            store = thunk();
            done = true;
        }
        return store;
    };
}
Array.prototype.sum = function () {
    let res = 0;
    for (const x of this)
        res += x;
    return res;
};
Array.prototype.insertSorted = function (x, compare) {
    let start = 0;
    let stop = this.length - 1;
    let middle = 0;
    while (start <= stop) {
        middle = Math.floor((start + stop) / 2);
        if (compare(this[middle], x) > 0)
            stop = middle - 1;
        else
            start = middle + 1;
    }
    this.splice(start, 0, x);
    return this;
};
Array.prototype.concatLength = function () {
    let res = 0;
    for (const x of this)
        res += x.length;
    return res;
};
Array.prototype.sortOn = function (f) {
    return this.map(x => pair(f(x), x)).sort(compareFst).map(snd);
};
Array.prototype.last = function () {
    return this[this.length - 1];
};
Array.prototype.maximum = function (def) {
    if (this.length === 0)
        return def;
    let res = this[0];
    for (let i = 1; i < this.length; i++)
        res = Math.max(res, this[i]);
    return res;
};
Array.prototype.minimum = function (def) {
    if (this.length === 0)
        return def;
    let res = this[0];
    for (let i = 1; i < this.length; i++)
        res = Math.min(res, this[i]);
    return res;
};
// Use JSX with el instead of React.createElement
// Originally from https://gist.github.com/sergiodxa/a493c98b7884128081bb9a281952ef33
// our element factory
function createElement(type, props, ...children) {
    const element = document.createElement(type);
    for (const name in props || {}) {
        if (name.substr(0, 2) === "on")
            element.addEventListener(name.substr(2), props[name]);
        else
            element.setAttribute(name, props[name]);
    }
    for (const child of children.flat(10)) {
        const c = typeof child === "object" ? child : document.createTextNode(child.toString());
        element.appendChild(c);
    }
    return element;
}
// How .tsx gets desugared
const React = { createElement };
function showFile(p) {
    if (p[1]) {
        return React.createElement("li", null,
            React.createElement("b", null, p[0]));
    }
    else {
        return React.createElement("li", null, p[0]);
    }
}
function reportDetails(profile, search) {
    const result = React.createElement("div", { class: "details" });
    const self = new Prop(0);
    search.event(xs => self.set(xs.mapProfile((p, _) => p.index).maximum()));
    const f = (i) => React.createElement("a", { onclick: () => self.set(i) }, profile[i].name);
    self.event(i => {
        const p = profile[i];
        const content = React.createElement("ul", null,
            React.createElement("li", null,
                React.createElement("b", null, "Name:"),
                " ",
                p.name),
            React.createElement("li", null,
                React.createElement("b", null, "Built:"),
                " ",
                showRun(p.built)),
            React.createElement("li", null,
                React.createElement("b", null, "Built last run:"),
                " ",
                showBool(p.builtLast)),
            React.createElement("li", null,
                React.createElement("b", null, "Changed:"),
                " ",
                showBool(p.changed)),
            React.createElement("li", null,
                React.createElement("b", null, "Execution time:"),
                showTime(p.execution)),
            React.createElement("li", null,
                React.createElement("b", null, "Cmds that wrote files I read:"),
                React.createElement("ol", null, p.writers.map(d => React.createElement("li", null, f(d))))),
            React.createElement("li", null,
                React.createElement("b", null, "Cmds that read files I wrote:"),
                React.createElement("ul", null, p.readers.map(d => React.createElement("li", null, f(d))))),
            React.createElement("li", null,
                React.createElement("b", null, "Cmds that I have a hazard with:"),
                React.createElement("ul", null, p.hazards.map(d => React.createElement("li", null, f(d))))),
            React.createElement("li", null,
                React.createElement("b", null, "Files I wrote (bold files changed last run):"),
                React.createElement("ul", null, p.filesWritten.map(f => showFile(f)))),
            React.createElement("li", null,
                React.createElement("b", null, "Files I read (bold files changed last run):"),
                React.createElement("ul", null, p.filesRead.map(f => showFile(f)))));
        $(result).empty().append(content);
    });
    return result;
}
function reportParallelism(profile) {
    // now simulate for -j1 .. -j24
    const plotData = [{ label: "Realistic (based on current dependencies)", data: [], color: "#3131a7" },
        { label: "Ideal (if no dependencies and perfect speedup)", data: [], color: "green" },
        { label: "Gap", data: [], color: "orange" }
    ];
    let threads1;
    for (let threads = 1; threads <= 24; threads++) {
        const taken = simulateThreads(profile, threads)[0];
        if (threads === 1)
            threads1 = taken;
        plotData[0].data.push([threads, taken]);
        plotData[1].data.push([threads, threads1 / threads]);
        plotData[2].data.push([threads, Math.max(0, taken - (threads1 / threads))]);
    }
    const plot = React.createElement("div", { style: "width:100%; height:100%;" });
    bindPlot(plot, new Prop(plotData), {
        xaxis: { tickDecimals: 0 },
        yaxis: { min: 0, tickFormatter: showTime }
    });
    return React.createElement("table", { class: "fill" },
        React.createElement("tr", null,
            React.createElement("td", { style: "text-align:center;" },
                React.createElement("h2", null, "Time to build at different number of threads"))),
        React.createElement("tr", null,
            React.createElement("td", { height: "100%" }, plot)),
        React.createElement("tr", null,
            React.createElement("td", { style: "text-align:center;" }, "Number of threads available.")));
}
// Simulate running N threads over the profile, return:
// [total time take, point at which each entry kicked off]
function simulateThreads(profile, threads) {
    // How far are we through this simulation
    let timestamp = 0;
    // Who is currently running, with the highest seconds FIRST
    const running = [];
    const started = [];
    // Things that are done
    const ready = profile.filter(x => x.writers.length === 0);
    const waiting = profile.map(x => x.writers.concatLength()); // number I am waiting on before I am done
    function runningWait() {
        const [ind, time] = running.pop();
        timestamp = time;
        for (const d of profile[ind].readers) {
            waiting[d]--;
            if (waiting[d] === 0)
                ready.push(profile[d]);
        }
    }
    while (true) {
        // Queue up as many people as we can
        while (running.length < threads && ready.length > 0) {
            const p = ready.pop();
            started[p.index] = timestamp;
            running.insertSorted([p.index, timestamp + p.execution], compareSndRev);
        }
        if (running.length === 0) {
            if (waiting.maximum(0) > 0)
                throw new Error("Failed to run all tasks");
            return [timestamp, started];
        }
        runningWait();
    }
}
function reportRuleTable(profile, search) {
    const columns = [{ field: "name", label: "Name", width: 400 },
        { field: "count", label: "Count", width: 65, alignRight: true, show: showInt },
        { field: "leaf", label: "Leaf", width: 60, alignRight: true },
        { field: "run", label: "Run", width: 50, alignRight: true },
        { field: "time", label: "Time", width: 75, alignRight: true, show: showTime }
    ];
    return newTable(columns, search.map(s => ruleData(s)), "time", true);
}
function ruleData(search) {
    return search.mapProfiles((ps, name) => ({
        name,
        count: ps.length,
        leaf: ps.every(p => p.writers.length === 0),
        run: ps.map(p => p.built).minimum(),
        time: ps.map(p => p.execution).sum()
    }));
}
function reportSummary(profile) {
    let sumExecution = 0; // build time in total
    const countTrace = profile.length;
    // start both are -1 because the end command will have run in the previous step
    for (const p of profile) {
        sumExecution += p.execution;
    }
    return React.createElement("div", null,
        React.createElement("h2", null, "Totals"),
        React.createElement("ul", null,
            React.createElement("li", null,
                React.createElement("b", null, "Rules:"),
                " ",
                showInt(profile.length),
                " ",
                React.createElement("span", { class: "note" }, "number of commands."))),
        React.createElement("h2", null, "Performance"),
        React.createElement("ul", null,
            React.createElement("li", null,
                React.createElement("b", null, "Build time:"),
                " ",
                showTime(sumExecution),
                " ",
                React.createElement("span", { class: "note" }, "how long a complete build would take single threaded.")),
            React.createElement("li", null,
                React.createElement("b", null, "Speculative critical path:"),
                " ",
                showTime(speculativeCriticalPath(profile)),
                " ",
                React.createElement("span", { class: "note" }, "how long it would take on infinite CPUs."))));
}
function speculativeCriticalPath(profile) {
    const criticalPath = []; // the critical path to any element
    let maxCriticalPath = 0;
    for (const p of profile) {
        let cost = 0;
        for (const d of p.writers)
            cost = Math.max(cost, criticalPath[d]);
        cost += p.execution;
        maxCriticalPath = Math.max(cost, maxCriticalPath);
        criticalPath[p.index] = cost;
    }
    return maxCriticalPath;
}
