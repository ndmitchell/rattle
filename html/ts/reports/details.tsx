
function showFile(p: [string, int]): HTMLElement {
    if (p[1] ) {
        return <li><b>{p[0]}</b></li>;
    } else {
        return <li>{p[0]}</li>;
    }
}

function reportDetails(profile: Profile[], search: Prop<Search>): HTMLElement {
    const result = <div class="details"></div>;
    const self: Prop<pindex> = new Prop(0);
    search.event(xs => self.set(xs.mapProfile((p, _) => p.index).maximum()));
    const f = (i: pindex) => <a onclick={() => self.set(i)}>{profile[i].name}</a>;
    self.event(i => {
        const p = profile[i];
        const content = <ul>
            <li><b>Name:</b> {p.name}</li>
            <li><b>Built:</b> {showRun(p.built)}</li>
            <li><b>Built last run:</b> {showBool(p.builtLast)}</li>
            <li><b>Changed:</b> {showBool(p.changed)}</li>
            <li><b>Execution time:</b>{showTime(p.execution)}</li>
            <li><b>Cmds that wrote files I read:</b>
                <ol>
                    {p.writers.map(d => <li>{f(d)}</li>)}
                </ol>
            </li>
            <li><b>Cmds that read files I wrote:</b>
                <ul>
                    {p.readers.map(d => <li>{f(d)}</li>)}
                </ul>
            </li>
            <li><b>Cmds that I have a hazard with:</b>
                <ul>
                    {p.hazards.map(d => <li>{f(d)}</li>)}
                </ul>
            </li>
            <li><b>Files I wrote (bold files changed last run):</b>
                <ul>
                    {p.filesWritten.map(f => showFile(f))}
                </ul>
            </li>
            <li><b>Files I read (bold files changed last run):</b>
                <ul>
                    {p.filesRead.map(f => showFile(f))}
                </ul>
            </li>
        </ul>;
        $(result).empty().append(content);
    });
    return result;
}
