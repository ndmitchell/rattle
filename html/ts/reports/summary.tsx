
function reportSummary(profile: Profile[]): HTMLElement {
    let sumExecution: seconds = 0; // build time in total
    const countTrace: int = profile.length;
        // start both are -1 because the end command will have run in the previous step

    for (const p of profile) {
        sumExecution += p.execution;
    }

    return <div>
        <h2>Totals</h2>
        <ul>
            <li><b>Rules:</b> {showInt(profile.length)} <span class="note">number of commands.</span></li>
        </ul>
        <h2>Performance</h2>
        <ul>
            <li><b>Build time:</b> {showTime(sumExecution)} <span class="note">how long a complete build would take single threaded.</span></li>
            <li><b>Speculative critical path:</b> {showTime(speculativeCriticalPath(profile))} <span class="note">how long it would take on infinite CPUs.</span></li>
        </ul>
    </div>;
}

function speculativeCriticalPath(profile: Profile[]): seconds {
    const criticalPath: seconds[] = []; // the critical path to any element
    let maxCriticalPath: seconds = 0;
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
