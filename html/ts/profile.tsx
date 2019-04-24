
function profileLoaded(profileRaw: ProfileRaw[]): void {
    $(document.body).empty().append(profileRoot(unraw(profileRaw)));
}

function unraw(xs: ProfileRaw[]): Profile[] {
    const ans = xs.map((x, i) => ({
        index: i,
        name: x[0],
        execution: x[1], // max time to build
        built: x[2], // number of times traced
        filesWritten: x[3],
        filesRead: x[4],
        readers: x[5], // rdepends
        writers: x[6], // depends
        hazards: x[7] // depends and rdepends that violate consistency
    } as Profile));
    return ans;
}

function profileRoot(profile: Profile[]): HTMLElement {
    const [s, search] = createSearch(profile);
    const t = createTabs(
        [ ["Summary", () => reportSummary(profile)]
        // , ["Command plot", () => reportCmdPlot(profile)]
        // , ["Commands", () => reportCmdTable(profile, search)]
        , ["Commands", () => reportRuleTable(profile, search)]
        , ["Parallelizability", () => reportParallelism(profile)]
        , ["Details", () => reportDetails(profile, search)]
        // , ["Why rebuild", () => reportRebuild(profile, search)]
        ]);
    return <table class="fill">
        <tr><td style="padding-top: 8px; padding-bottom: 8px;">
            <a href="https://github.com/ndmitchell/rattle" style="font-size: 20px; text-decoration: none; color: #3131a7; font-weight: bold;">
                Rattle profile report
            </a>
            <span style="color:gray;white-space:pre;">   - generated at {generated} by Rattle v{version}</span>
        </td></tr>
        <tr><td>{s}</td></tr>
        <tr><td height="100%">{t}</td></tr>
    </table>;
}


function createTabs(xs: Array<[string, () => HTMLElement]>): HTMLElement {
    const bodies: Array< [HTMLElement, () => void] > = xs.map(x => {
        const el = <div style="padding:5px;width:100%;height:100%;min-width:150px;min-height:150px;overflow:auto;display:none;"></div>;
        const upd = lazy(() => $(el).append(x[1]()));
        return pair(el, upd);
    });
    let lbls = [];
    const f = (i: int) => () => {
        bodies[i][1]();
        lbls.map((x, j) => $(x).toggleClass("active", i === j));
        bodies.map((x, j) => $(x[0]).toggle(i === j));
        $(window).trigger("resize");
    };
    lbls = xs.map((x, i) => <a onclick={f(i)}>{x[0]}</a>);
    f(0)();
    return <table class="fill">
        <tr><td>
            <table width="100%" style="border-spacing:0px;"><tr class="tabstrip">
                <td width="20" class="bottom">&nbsp;</td>
                <td style="padding:0px;">{lbls}</td>
                <td width="100%" class="bottom">&nbsp;</td>
            </tr></table>
        </td></tr>
        <tr height="100%">
            <td style="background-color:white;">
                {bodies.map(fst)}
            </td>
        </tr>
    </table>;
}
