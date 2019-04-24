// Stuff that Shake generates and injects in

// The version of Shake
declare const version: string;
declare const generated: string;

/////////////////////////////////////////////////////////////////////
// PROFILE DATA

type timestamp = int;

type pindex = int; // an index into the list of profiles

interface Profile {
    index: pindex; // My index in the list of profiles
    name: string; // Name of the thing I built
    execution: seconds; // Seconds I took to execute
    built: int; // number of times built
    filesWritten: string[]; // files written
    filesRead: string[]; // files read
    readers: pindex[]; // cmds that depend on me (aka they read file(s) i wrote)
    writers: pindex[]; // cmds I depend on (aka i read file(s) they wrote)
    hazards: pindex[]; // cmds that violate consistency with this cmd.
}

function untraced(p: Profile): seconds {
    return 0;
}

type ProfileRaw =
    [ string
    , seconds
    , int
    , string[]
    , string[]
    , pindex[]
    , pindex[]
    , pindex[]
    ];

/////////////////////////////////////////////////////////////////////
// BASIC UI TOOLKIT

class Prop<A> {
    private val: A;
    private callback: ((val: A) => void);
    constructor(val: A) { this.val = val; this.callback = () => { return; }; }
    public get(): A { return this.val; }
    public set(val: A): void {
        this.val = val;
        this.callback(val);
    }
    public event(next: (val: A) => void): void {
        const old = this.callback;
        this.callback = val => { old(val); next(val); };
        next(this.val);
    }
    public map<B>(f: (val: A) => B): Prop<B> {
        const res = new Prop(f(this.get()));
        this.event(a => res.set(f(a)));
        return res;
    }
}
