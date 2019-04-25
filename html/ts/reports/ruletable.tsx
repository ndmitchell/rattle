
function reportRuleTable(profile: Profile[], search: Prop<Search>): HTMLElement {
    const columns: Column[] =
        [ {field: "name", label: "Name", width: 400}
        , {field: "count", label: "Count", width: 65, alignRight: true, show: showInt}
        , {field: "leaf", label: "Leaf", width: 60, alignRight: true}
        , {field: "run", label: "Run", width: 50, alignRight: true}
        , {field: "time", label: "Time", width: 75, alignRight: true, show: showTime}
        ];
    return newTable(columns, search.map(s => ruleData(s)), "time", true);
}

function ruleData(search: Search): object[] {
    return search.mapProfiles((ps, name) => ({
        name,
        count: ps.length,
        leaf: ps.every(p => p.writers.length === 0),
        run: ps.map(p => p.built).minimum(),
        time: ps.map(p => p.execution).sum()
    }));
}
