// ============================================================
// app.js -- Alpine component that drives the config builder,
// live previews, the R run (SSE), and results display.
// No statistics here; everything numeric comes from the backend.
// ============================================================

const OPS = ["in","notin","==","!=","<","<=",">",">=","between",
             "abs_le","abs_ge","abs_between","contains","not_contains","isna","notna"];
const SECTIONS = ["baseline","preop","outcome"];
const SECTION_TITLES = {baseline:"Baseline", preop:"Preop / Intraop", outcome:"Outcome"};
const TYPES = ["continuous","ordinal","dichotomous","categorical"];
const ROLE_LABELS = {
  pre_ucva:"Pre UCVA", pre_bcva:"Pre BCVA", post_ucva:"Post UCVA", post_bcva:"Post BCVA",
  pre_sph:"Pre Sphere", pre_cyl:"Pre Cylinder", pre_axis:"Pre Axis",
  post_sph:"Post Sphere", post_cyl:"Post Cylinder", post_axis:"Post Axis",
  pre_seq:"Pre SEQ", post_seq:"Post SEQ", target_seq:"Target SEQ (optional)", eye:"Eye (Alpins)"
};
// preferred "positive"/event levels for the GUI's first guess: the same
// entries, in the same order, as R default_positive_level() in R/helpers.R (the
// fallback of pick_display_level / make_binary_01), which also applies the
// DICHO_NEG complement rule below. The guess is written into the config, so R
// then uses exactly the level shown here.
const DICHO_PREFS = ["F","Female","female","1","Yes","yes","YES","Y","y","True","true","TRUE",
                     "Present","present","Positive","positive","Abnormal","abnormal",
                     "M","Male","male"];
// common NON-event codings: with exactly two levels and one of them listed
// here, the other one is the natural event
const DICHO_NEG = ["0","No","no","NO","N","n","None","none","Normal","normal","False","false","FALSE",
                   "Absent","absent","Negative","negative"];
// R .level_key(): case and surrounding spaces ignored, numbers in canonical
// form ("1.0" -> "1"), TRUE / FALSE spelled as 1 / 0
function levelKey(z){
  const s=rTrim(z==null ? "" : z).toUpperCase();
  if(s!=="" && isFinite(Number(s))) return String(Number(s));
  return s==="TRUE" ? "1" : (s==="FALSE" ? "0" : s);
}
// R resolve_positive_level(): the data level a preferred level refers to, or
// null when it does not resolve to exactly one. Tried in order: exact text,
// case/space-insensitive text, then the normalised key (1 = 1.0 = TRUE = True).
function resolveLevel(pref, values){
  if(pref===null || pref===undefined || !rTrim(pref)) return null;
  const p=String(pref), ch=[...new Set((values||[]).map(String))];
  for(const same of [c=>c===p,
                     c=>rTrim(c).toUpperCase()===rTrim(p).toUpperCase(),
                     c=>levelKey(c)===levelKey(p)]){
    const hit=ch.filter(same);
    if(hit.length===1) return hit[0];
  }
  return null;
}
// visual-acuity notations understood by the engine (config key va_format)
const VA_FORMATS = [["decimal","Decimal (1.0 = 20/20)"],["logmar","logMAR (0.0 = 20/20)"],
                    ["snellen","Snellen fraction (20/40, 6/12)"],["etdrs","ETDRS letter score (85 = 20/20)"]];
const VA_ROLES = ["pre_ucva","pre_bcva","post_ucva","post_bcva"];
// the plus-to-minus cylinder sets are pre-filled from these roles (pre set, post set)
const STD_ROLE_SETS = [["pre_sph","pre_cyl","pre_axis"],["post_sph","post_cyl","post_axis"]];
// MatchIt methods that run with the bundled R library; a preset asking for
// any other method loads with a warning and falls back to nearest
const PSM_METHODS = [["nearest","nearest neighbor"],["exact","exact"],
                     ["cem","coarsened exact (cem)"],["subclass","subclassification"]];
// matching distances that are not propensity scores (config_schema
// _NON_PS_DISTANCES): MatchIt takes no caliper in score SDs and no
// largest / smallest match order with them, and subclass cannot use them
const NON_PS_DISTANCES = ["mahalanobis","robust_mahalanobis","euclidean","scaled_euclidean"];
// must equal APP_VERSION in server/app.py — mismatch means the running server
// process is older than the page and needs a restart
const UI_VERSION = "2026-10-04-se.10";
// manuscript group colors (mEYEstro-like); index = group order
const DEFAULT_GROUP_COLORS = ["#5B2C86","#F26522","#1B9E4B","#2166AC","#00838F","#C2185B"];

function emptyTyping(){ return {continuous:[],ordinal:[],dichotomous:[],categorical:[],order:[]}; }

// Reusable searchable single-select. get/set are closures into parent state;
// opts is an array or a function returning one. none=true when the list
// starts with a "- none -" entry (it clears the value); that entry is
// highlight position -1, so it is reachable from the keyboard like any option.
// Keyboard: ArrowDown / ArrowUp move the highlight (idx), Enter picks it,
// Escape closes, Tab closes and moves on (Home / End stay with the text box).
// Focusing the box highlights the CURRENT value, and Enter picks only after an
// arrow key moved the highlight or a filter was typed: a bare Enter in a
// mapped box never remaps it.
function combo(get, set, opts, none){
  const lo = none ? -1 : 0;
  return {
    open:false, q:"", idx:0, moved:false,
    val(){ const v=get(); return (v===null||v===undefined)?"":String(v); },
    list(){
      let o = (typeof opts==="function"?opts():opts)||[];
      const q=this.q.trim().toLowerCase();
      return q ? o.filter(c=>String(c).toLowerCase().includes(q)) : o;
    },
    // highlight position of the current value (-1: "- none -" / nothing)
    curIdx(){ const v=this.val(); return v==="" ? -1 : this.list().findIndex(c=>String(c)===v); },
    onFocus(){ this.open=true; this.q=""; this.moved=false; this.idx=this.curIdx(); },
    choose(v){ set(v); this.open=false; this.q=""; this.idx=0; this.moved=false; },
    key(e){
      const L=this.list();
      const hi=L.length-1;   // -1 when nothing matches: only "- none -" (if any) is left
      const move=to=>{
        this.idx=Math.max(lo, Math.min(Math.max(hi, lo), to));
        // keep the highlighted option visible in the scrolling list
        const box=e.target && e.target.closest ? e.target.closest(".combo") : null;
        setTimeout(()=>{ const a=box && box.querySelector(".combo-item.active");
                         if(a && a.scrollIntoView) a.scrollIntoView({block:"nearest"}); }, 0);
      };
      if(e.key==="ArrowDown" || e.key==="ArrowUp"){
        e.preventDefault();
        this.moved=true;
        if(!this.open){ const ci=this.curIdx(); this.open=true; this.idx=(ci>=0 ? ci : 0); return; }
        if(!L.length && !none) return;
        // a filter that matches nothing leaves the highlight on "- none -"
        if(!L.length){ move(lo); return; }
        move(this.idx + ((e.key==="ArrowDown") ? 1 : -1));
      } else if(e.key==="Enter"){
        if(!this.open) return;
        // nothing chosen yet (no arrow key, no filter): keep the current value
        if(!this.moved && !this.q.trim()){ e.preventDefault(); this.open=false; return; }
        if(none && this.idx<0){ e.preventDefault(); this.choose(""); return; }
        if(L.length){ e.preventDefault(); this.choose(L[Math.max(0, Math.min(this.idx, L.length-1))]); }
      } else if(e.key==="Escape"){
        if(this.open) e.preventDefault();
        this.open=false; this.q=""; this.idx=0; this.moved=false;
      } else if(e.key==="Tab"){
        this.open=false; this.q=""; this.idx=0; this.moved=false;
      }
    },
  };
}

// R's trimws(): spaces, tabs and line breaks only
const rTrim = x => String(x).replace(/^[ \t\r\n]+|[ \t\r\n]+$/g, "");
// Same rule as safe_filename() in R/helpers.R: characters Windows forbids in
// file names become "_", control characters (C0, DEL and C1) are dropped, at most 100
// characters, trailing dots / spaces are dropped, and a reserved device name -
// judged on the part before the FIRST dot, trailing spaces ignored (CON, NUL,
// COM1, CONIN$, ...) - gets "_" right after that part ("NUL.v2" -> "NUL_.v2").
// Unlike R, an empty result stays "" here (callers decide what to show).
function safeFilename(x){
  let s=String(x==null ? "" : x).replace(/[<>:"\/\\|?*]+/g, "_").replace(/[\x00-\x1f\x7f-\x9f\u2028\u2029]+/g, "");
  s=rTrim(s);
  const ch=Array.from(s);
  if(ch.length>100) s=ch.slice(0,100).join("");
  s=s.replace(/[. ]+$/, "");
  const stem=s.replace(/\..*$/, "").replace(/ +$/, "");
  if(/^(CON|PRN|AUX|NUL|CONIN\$|CONOUT\$|COM[1-9]|LPT[1-9])$/i.test(stem)) s=s.replace(/^([^. ]*)/, "$1_");
  return s;
}
// run names the engine uses for the combined runs
const RESERVED_RUN_NAMES = ["pooled","all"];

// ---- column names exactly as the engine resolves them (R/helpers.R) ----
// R's reserved words: make.names() appends "." to them
const R_RESERVED = new Set(["if","else","repeat","while","function","for","in","next","break","TRUE","FALSE",
  "NULL","Inf","NaN","NA","NA_integer_","NA_real_","NA_character_","NA_complex_"]);
// .clean_stem(): ASCII letters / digits joined by "_"
function cleanStem(x){ return rTrim(x==null ? "" : x).replace(/[^A-Za-z0-9]+/g, "_").replace(/^_|_$/g, ""); }
// make.names() of one stem (the stem holds only letters, digits and "_")
function makeName(s){ if(!/^[A-Za-z]/.test(s)) s="X"+s; return R_RESERVED.has(s) ? s+"." : s; }
// .name_key(): the case/punctuation-insensitive key; "" when the name has no
// ASCII letter or digit (such a name is only ever matched exactly)
function colKey(x){ const s=cleanStem(x); return s ? makeName(s).toLowerCase() : ""; }
// .resolve_one() in R/helpers.R (= Resolver.resolve in server/preview.py):
// {col, ambiguous}. Tiers: (1) the exact header; (2) the request without its
// surrounding spaces, again as an exact header; (3) the key, ONLY when exactly
// one header has it - two or more = ambiguous = not resolved.
const COL_INDEX = new WeakMap();
function resolveColumn(cols, req){
  const out={col:null, ambiguous:[]};
  if(req===null || req===undefined) return out;
  req=String(req);
  if(!rTrim(req)) return out;
  let ix=COL_INDEX.get(cols);
  if(!ix){
    const raw=cols.map(c=>c==null ? "" : String(c));
    ix={raw, keys:raw.map(colKey)};
    COL_INDEX.set(cols, ix);
  }
  const raw=ix.raw;
  if(raw.includes(req)){ out.col=req; return out; }
  if(raw.includes(rTrim(req))){ out.col=rTrim(req); return out; }
  const k=colKey(req);
  if(!k) return out;
  const hits=raw.filter((c,j)=>ix.keys[j]===k);
  if(hits.length===1) out.col=hits[0];
  else if(hits.length>1) out.ambiguous=hits;
  return out;
}

// One pristine copy of the config-bearing state. Loading a preset REPLACES
// the state with a fresh copy of this and merges the preset on top, so an
// older or partial preset can never inherit the previous study's settings.
// Stable x-for keys for every repeatable row (groups, filters, subsets, std
// triples, stability visits). _uid is UI-only and never reaches the config.
let GROUP_UID = 0;
const withUid = o => ({...o, _uid:++GROUP_UID});
function defaultState(){
  const g1={name:"Group1", values:[], _uid:++GROUP_UID}, g2={name:"Group2", values:[], _uid:++GROUP_UID};
  return  {
      guess_max: 120000,
      patient_id_raw:"", eye_id_raw:"", group_source_var:"",
      groups: [ g1, g2 ],
      // The reference group is an IDENTITY (the row's _uid), never a name:
      // renaming, clearing or duplicating names cannot move it. buildCfg writes
      // its current name; applyCfg maps a saved name back to a row.
      reference_uid: g1._uid,
      reference_orphan: "",    // a config's reference name that matches no group (shown, never guessed)
      // read-only name view of the reference, plus set-by-name (scripts, tests)
      get reference_group(){
        const g=this.groups.find(x=>x._uid===this.reference_uid);
        return g ? g.name : "";
      },
      set reference_group(n){
        const blank = n===null || n===undefined || String(n)==="";
        const g = blank ? null : this.groups.find(x=>x.name===String(n));
        this.reference_uid = g ? g._uid : null;
        this.reference_orphan = (g || blank) ? "" : String(n);
      },
      patient_level_params_raw: [], exclude_params_raw: [],
      // nothing is inferred from column names: summary roles start unmapped
      age_var:"", fu_var:"", sex_var:"",
      baseline: emptyTyping(), preop: emptyTyping(), outcome: emptyTyping(),
      within_group: emptyTyping(),   // distribution-only, summarized inside each group
      within_levels: {},             // column -> "eye" | "patient" (unit of analysis)
      filters: [],
      do_matching:true, psm_covariates_raw:[],
      // 1:1 without replacement, caliper 0.2 SD of the LOGIT of the propensity
      // score (decision D13, Austin 2011): the conventional design, and the one
      // that keeps matching weights at 1; "logit" / "glm" match on the
      // probability scale (v1.0.0's default, still selectable)
      psm_method:"nearest", psm_distance:"linear.logit", psm_ratio:1,
      psm_replace:false, psm_caliper:0.2, psm_std_caliper:true, psm_m_order:"largest",
      psm_seed:20250601,       // fixed seed: random match order / ties give the same cohort every run
      run_gee_stage:"matched", run_main_gee:true, run_adjusted_gee:false,
      gee_adjust_covariates_raw:[], use_matching_weights_in_gee:true,
      add_gee_columns:true, gee_source_for_tables:"main",
      // small-sample GEE inference (D11: "md" = Mancl-DeRouen + t / F, "none" =
      // v1.0.0) and the summary tables' tests (D12: "auto" = patient-clustered
      // when a patient has two analysed eyes, "classical" = v1.0.0)
      gee_small_sample:"md", table_tests:"auto",
      // subset_mode: "separate" (one workbook per subset) | "both" (+ pooled) | "pooled" (pooled only)
      subset_col_raw:"", subsets:[], subset_mode:"separate",
      do_standardize_minus_cyl:true, std_triples:[],
      va_format:"decimal",     // notation of every VA-role / logmar column, see VA_FORMATS
      // acuity analysed in logMAR by default (D16); the column list follows the
      // mapped VA roles until the user edits it (seedLogmarCols)
      do_logmar:true, logmar_cols_raw:[],
      make_meyestro:true, make_alpins:true, make_pub_table:true,
      make_graphs:true, graph_set:"nine", graph_formats:["tiff"], graph_dpi:300,
      // figure style (D20: "journal" | "annotated" = v1.0.0), figure width in cm
      // (D21: "" = each figure's own size; 8.76 / 12.69 / 17.77 = journal
      // columns) and the P value style of tables, sheets and figures (D15)
      figure_style:"journal", figure_width_cm:"", p_style:"ama",
      stability_figs:false,
      stability_visits:[], stability_include_preop:true,
      stability_change_from:"", stability_change_to:"", stability_change_threshold:0.50,
      astig_panels:true, double_angle_figs:true, semicircle_figs:true,
      // the "Astigmatism (JCRS 2025)" + "Astigmatism vectors" sheets (D8); the
      // 95% region of the eyes ("polygon" = the depth-based convex polygon as
      // in the WHWK functions, R3-1) and of each centroid ("bootstrap" = the
      // bootstrap convex polygon as in the WHWK functions | "hotelling", R3-2);
      // the WHWK input files beside the double-angle figures (R3-3)
      astig_sheet:true, astig_region:"polygon", astig_centroid_region:"bootstrap",
      astig_whwk_files:true,
      astig_jcrs_standard:true, cluster_by_patient:true, astig_test_method:"robust",
      astig_vector_cols:{}, astig_target:{},
      do_regression:true, regression_stage:"unmatched", regression_covariates_raw:[],
      roles: Object.fromEntries(Object.keys(ROLE_LABELS).map(k=>[k,""])),
      dicho_map: {},           // column -> positive level (GUI-driven)
      output_dir:"", output_prefix:"StandardEyes", export_filtered_csv:true,
    };
}

function builder(){
  return {
    // ---- meta ----
    tab: "data",
    tabs: [
      ["data","Data & Groups"],["vars","Variables & Types"],["filters","Filters & Subsets"],
      ["match","Matching"],["regress","Regression"],["gee","GEE"],
      ["special","Special outputs"],["run","Run & Results"]
    ],
    ROLE_LABELS, OPS, SECTIONS, SECTION_TITLES, TYPES,

    // ---- data ----
    cols: [], sheets: [], file: null, sheet: null, nrows:0, ncols:0,
    preview: {columns:[],rows:[]},
    sortCol:"", sortDir:"asc",
    pathInput: "",
    groupSourceValues: [], subsetColValues: [],
    distinctCache: {},
    // /api/distinct returns at most 1000 values; columns cut there are flagged
    // here and their search boxes ask the server ({q, values} per column)
    distinctTrunc: {}, distinctQ: {},
    renamedHeaders: [],   // blank / repeated headers shown under generated names
    similarHdrs: null,    // /api/load similar_headers (null: compute locally with the engine's key)
    serverDown:false,     // /api/health unreachable (retried every 3 s)
    stabilityMsg:"",      // feedback of "Suggest visits from column names"

    // ---- config-bearing state ----
    s: defaultState(),

    // drag state for order chips
    drag: {sec:null, i:null},

    // ---- live histogram / within-group analysis zone ----
    hist: {col:"", data:null, busy:false, error:null, stage:"filtered", group:"", mfile:""},

    // ---- counts / run / results ----
    counts: null, countsBusy:false, countsError:null,
    running:false, progress:0, progressMsg:"", log:[], runError:null,
    cancelling:false, serverBusy:false,      // serverBusy: a run started elsewhere is still active
    results:null, resultSheet:{file:null,sheet:null,data:null,error:null},
    presets:[], presetName:"", activePreset:"",
    previewOnly:false,                       // run without saving to the output folder
    // reason: why the preview cannot run with the current settings ("" = it can)
    matchprev:{busy:false, data:null, error:null, stale:true, run:"", reason:""},
    fprev:{busy:false, data:null, error:null, sortCol:"", sortDir:"asc", stale:true},
    savedFiles:{}, exporting:false, exportMsg:null, exportOk:true,
    filteredExport:{busy:false, path:null, rows:null, error:null},
    figures:{busy:false, dirs:[], error:null},  // low-res PNG previews of the last run's figures
    dichoGuessSrc:{},     // column -> "pref" | "complement" | "fallback" | "user" (how the level was chosen)
    dichoReplaced:{},     // column -> level from a preset that is not in this file (replaced by a guess)
    dichoUnseen:{},       // column -> kept preset level that does not occur in a single-valued column (0 events)
    vaGuess:{},           // VA column -> notation guessed by /api/va-detect (null = undecided)
    vaWarn:[],            // /api/va-detect warnings for the declared notation (e.g. ETDRS declared, decimal recorded)
    VA_FORMATS, PSM_METHODS,
    psmMethodNote:"",     // shown under the Method select when a config's method was replaced by nearest
    _stdNoSeed:false,     // true: the cylinder sets came from a config or the user removed one
    _logmarUser:false,    // true: the logMAR column list was chosen (edited, or kept from a config)
    // Request sequencing: each fetcher bumps its counter before sending and
    // drops any reply whose number is no longer the latest, so a slow older
    // response can never overwrite a fresher one. The *Gen counters count
    // settings changes, so a reply for superseded settings stays marked stale.
    _histSeq:0, _cntSeq:0, _mpSeq:0, _fpSeq:0, _sheetSeq:0, _prevSeq:0, _vaSeq:0, _figSeq:0, _dqSeq:0,
    _mpGen:0, _fpGen:0,
    _fileGen:0,            // bumped whenever another sheet / file is bound (stale per-column replies are dropped)
    _busyT:null, _busy409:false,   // polling while a run started elsewhere is active
    _runCtl:null,          // AbortController of the /api/run stream
    _pendingSheet:null,    // sheet named by a preset loaded before its workbook
    // true once a config (preset / JSON) has been applied: subsets then come
    // from that config and are never auto-seeded, even when it has none
    _subsetsFromCfg:false,

    // field lists used to render combos generically
    ID_FIELDS: [
      {key:"patient_id_raw", label:"Patient / cluster ID"},
      {key:"eye_id_raw", label:"Eye / case ID"},
      {key:"group_source_var", label:"Group source column"},
    ],
    SUMMARY_FIELDS: [
      {key:"age_var", label:"Age"},
      {key:"fu_var", label:"Follow-up time"},
      {key:"sex_var", label:"Sex"},
    ],
    // Plus-to-minus cylinder sets are never guessed from column names. Once the
    // pre-op AND post-op sphere / cylinder / axis roles are all mapped and no
    // set exists yet, the two sets are pre-filled from those roles; otherwise
    // the list stays empty. Sets the user removed, and the sets a config
    // carries, are never re-seeded; a config's empty list is seeded unless
    // the user had emptied it (std_triples_user).
    addStdTriple(){ this.s.std_triples.push(withUid({sph:"",cyl:"",axis:""})); },
    removeStdTriple(i){ this.s.std_triples.splice(i,1); this._stdNoSeed=true; },
    // which of the settings next to the Method select the chosen method uses
    psmMethodHint(m){
      const tail=" Ratio, match order, replacement, caliper and seed are not used (grayed out). The live matched-count preview below covers nearest only; the run itself uses this method.";
      return {
        exact: "Exact matching keeps the eyes whose covariate values exactly equal those of an eye in the other group, so it needs discrete covariates (sex, eye, a category): with a continuous covariate such as age or refraction almost no eye has an exact twin and no eye is matched — use coarsened exact (cem) for those. The distance is not used either, and the matched eyes carry weights that are generally not 1."+tail,
        cem: "Coarsened exact matching bins each covariate and keeps the eyes that share a bin with the other group. The distance is not used, and the matched eyes carry weights that are generally not 1."+tail,
        subclass: "Subclassification splits the eyes into strata of the propensity score (built with the distance chosen) and weights them within each stratum; the weights are generally not 1."+tail,
      }[m] || "";
    },
    // does the chosen matching method use this setting? (ignored inputs are disabled)
    psmUses(field){
      const m=this.s.psm_method;
      if(m==="nearest" || !PSM_METHODS.some(x=>x[0]===m))
        return !((field==="caliper" || field==="std_caliper") && this.psmNonPS());
      if(field==="distance") return m==="subclass";
      return !["ratio","order","replace","caliper","std_caliper","seed"].includes(field);
    },
    // the chosen distance is not a propensity score (mahalanobis, ...)
    psmNonPS(){ return NON_PS_DISTANCES.includes(rTrim(this.s.psm_distance??"").toLowerCase()); },
    // options the server accepts with the chosen method / distance (others are greyed out)
    psmDistanceOk(o){ return !(this.s.psm_method==="subclass" && NON_PS_DISTANCES.includes(o)); },
    psmOrderOk(o){ return !(this.psmNonPS() && (o==="largest" || o==="smallest")); },
    // nearest matching on a distance that is not a propensity score: the
    // caliper (in SDs of the score) and the score-ranked orders cannot apply,
    // so they are cleared / set to 'data' (MatchIt's own default there).
    // Returns what was changed ("" = nothing), for the note under the select.
    fitPsmToDistance(){
      const s=this.s;
      if(s.psm_method!=="nearest" || !this.psmNonPS()) return "";
      const done=[];
      if(s.psm_caliper!==null && s.psm_caliper!==undefined && s.psm_caliper!==""){
        s.psm_caliper=null; done.push("the caliper was cleared"); }
      if(s.psm_m_order==="largest" || s.psm_m_order==="smallest"){
        s.psm_m_order="data"; done.push("the match order was set to data"); }
      return done.length ? "Distance “"+s.psm_distance+"” is not a propensity score, so "+done.join(" and ")+"." : "";
    },
    seedStdTriples(){
      if(this.s.std_triples.length || this._stdNoSeed) return;
      const r=this.s.roles||{};
      const sets=STD_ROLE_SETS.map(([sp,cy,ax])=>({sph:r[sp]||"", cyl:r[cy]||"", axis:r[ax]||""}));
      if(!sets.every(t=>t.sph && t.cyl && t.axis)) return;
      this.s.std_triples=sets.map(t=>withUid(t));
    },
    // logMAR columns (decision D16): with the switch on, the list is the mapped
    // VA role columns - kept in step with the roles - until the user edits it
    // (_logmarUser), the rule the engine applies to a settings file without a
    // list. Only an actual change is written: a watcher tracks these arrays.
    seedLogmarCols(){
      if(!this.s.do_logmar || this._logmarUser) return;
      const r=this.s.roles||{}, want=[];
      for(const k of VA_ROLES){ const c=r[k]; if(c && !want.includes(c)) want.push(c); }
      if(JSON.stringify(want)!==JSON.stringify(this.s.logmar_cols_raw)) this.s.logmar_cols_raw=want;
    },
    toggleLogmar(c){ this._logmarUser=true; this.toggle(this.s.logmar_cols_raw, c); },
    // the VA role columns the logMAR list does not hold (shown under the list)
    logmarRolesLeftOut(){
      const r=this.s.roles||{};
      return VA_ROLES.map(k=>r[k]).filter((c,i,a)=>c && a.indexOf(c)===i && !this.s.logmar_cols_raw.includes(c));
    },

    // ---- stability-over-time visit mapping ----
    // The list starts EMPTY and nothing is ever filled in automatically: column
    // naming is a local convention, so the user maps each visit explicitly.
    addStabilityVisit(){ this.stabilityMsg=""; this.s.stability_visits.push(withUid({label:"",time:"",seq:"",sph:"",cyl:""})); },
    removeStabilityVisit(i){
      const gone=this.s.stability_visits.splice(i,1)[0];
      for(const k of ["stability_change_from","stability_change_to"])
        if(gone && this.s[k]===gone.label) this.s[k]="";
    },
    stabilityLabels(){ return this.s.stability_visits.map(v=>v.label).filter(l=>l!==""); },
    // the change-from / change-to pair names visits by label, so with two
    // visits sharing a label it cannot tell which one is meant; the run is
    // blocked while this is non-empty
    stabilityLabelDupes(){
      if(!this.s.stability_figs) return [];
      const L=this.stabilityLabels().map(l=>String(l).trim());
      return [...new Set(L.filter((l,i)=>L.indexOf(l)!==i))];
    },
    // the same column mapped in two visits usually means one row duplicates another
    stabilityDupes(){
      const seen={}, dup=new Set();
      for(const v of this.s.stability_visits)
        for(const c of [v.time,v.seq,v.sph,v.cyl])
          if(c){ if(seen[c]) dup.add(c); seen[c]=1; }
      return [...dup];
    },
    // Convenience only, never automatic: propose one row per column family that
    // has a post-op day column plus a SEQ (or sphere + cylinder) sharing its name.
    ORDINALS: ["first","second","third","fourth","fifth","sixth"],
    // approximate days after surgery a visit label stands for ("1M" 30, "week 1" 7,
    // "12" 12); ordinal words sort after every number; NaN when nothing is recognisable
    visitRank(label){
      const l=String(label||"").toLowerCase();
      const UNIT={d:1,day:1,days:1,w:7,wk:7,wks:7,week:7,weeks:7,m:30.44,mo:30.44,mos:30.44,
                  mon:30.44,month:30.44,months:30.44,y:365.25,yr:365.25,yrs:365.25,year:365.25,years:365.25};
      const m=l.match(/(\d+(?:\.\d+)?)\s*([a-z]+)?/);
      if(m){
        let unit=m[2];
        if(!(unit in UNIT)){ const m2=l.match(/([a-z]+)\s*\d/); unit=m2 ? m2[1] : ""; }
        return parseFloat(m[1])*(UNIT[unit]||1);
      }
      const o=this.ORDINALS.findIndex(w=>l.includes(w));
      return o>=0 ? 1e6+o : NaN;
    },
    suggestStabilityVisits(){
      this.stabilityMsg="";
      const norm=c=>String(c).toLowerCase().replace(/[^a-z0-9]+/g,"");
      // the words of a header: split at separators, camelCase humps and a "Days" glued to a number
      const words=c=>String(c).replace(/([a-z])([A-Z])/g,"$1 $2").replace(/(days?)(?=\d)/gi,"$1 ")
                              .split(/[^A-Za-z0-9]+/).filter(Boolean);
      // whole words that say "time after surgery" rather than which visit it is
      const TIME=/^(days?|postop|preop|post|pre|op|surgery|surg|followup|follow|up|fu|after|from|since)$/i;
      const isSeq=n=>/seq|sphericalequivalent|sphequiv/.test(n);
      const isSph=n=>n.includes("sph") && !isSeq(n);
      const isCyl=n=>n.includes("cyl");
      const out=[], seen={};
      for(const t of this.cols){
        const n=norm(t);
        if(!(n.includes("day") && /post|op|surg|fu|follow/.test(n))) continue;
        // the visit's own name keeps its unit: "1M Post Days" -> "1M", "1Y ..." -> "1Y";
        // a "Day" next to a bare number is the visit's unit ("Day 1"), not the time column's
        const W=words(t), num=w=>/^\d+$/.test(w||"");
        const lab=W.filter((w,i)=>!TIME.test(w) || (/^days?$/i.test(w) && num(W[i+1])));
        if(!lab.length) continue;
        const lw=lab.map(w=>w.toLowerCase()), k=lw.join("");
        if(seen[k]) continue;
        const pickFrom=fam=>{
          const pick=f=>fam.find(c=>c!==t && f(norm(c)))||"";
          return {seq:pick(isSeq), sph:pick(isSph), cyl:pick(isCyl)};
        };
        // family = headers carrying every word of the visit name as a whole word
        // ("1M" never pulls in "11M"); headers without word breaks fall back to
        // a substring match
        let m=pickFrom(this.cols.filter(c=>{ const cw=words(c).map(w=>w.toLowerCase()); return lw.every(w=>cw.includes(w)); }));
        if(!(m.seq || (m.sph && m.cyl))) m=pickFrom(this.cols.filter(c=>norm(c).includes(k)));
        if(!(m.seq || (m.sph && m.cyl))) continue;
        seen[k]=1;
        out.push({k, rank:this.visitRank(lab.join(" ")), row:{label:lab.join(" "), time:t, ...m}});
      }
      // a "chosen"/"last" family points at whichever concrete visit it selected,
      // so drop it when real visits were found rather than plotting it twice
      const concrete=out.filter(o=>!/chosen|last/.test(o.k));
      const use=concrete.length>=2 ? concrete : out;
      use.sort((a,b)=>{
        const A=a.rank, B=b.rank;
        if(isNaN(A) && isNaN(B)) return 0;
        if(isNaN(A)) return 1;
        if(isNaN(B)) return -1;
        return A-B;
      });
      if(!use.length){
        // shown next to the button; runError is reserved for a failed analysis
        this.stabilityMsg="No follow-up column families were recognized — add the visits manually.";
        return;
      }
      // R pairs the change-from / change-to visits by label: never propose two equal labels
      const used=new Set();
      for(const o of use){
        let L=o.row.label, i=2;
        while(used.has(L.toLowerCase())) L=o.row.label+" ("+(i++)+")";
        used.add(L.toLowerCase()); o.row.label=L;
      }
      this.s.stability_visits = use.map(o=>withUid(o.row));
      const L=this.stabilityLabels();
      if(L.length>1){
        if(!this.s.stability_change_from) this.s.stability_change_from=L[Math.max(0,L.length-2)];
        if(!this.s.stability_change_to)   this.s.stability_change_to=L[L.length-1];
      }
    },

    // ================= init =================
    init(){
      this.checkVersion();
      this.refreshPresets();
      // groups enter as name + values only: a figure colour or figure label
      // changes no count, filter or match
      this.$watch(() => JSON.stringify([
        this.s.filters, this.groupKey(), this.s.subsets, this.s.exclude_params_raw,
        this.s.group_source_var, this.s.subset_col_raw, this.s.do_standardize_minus_cyl,
        this.s.std_triples,
        this.s.do_logmar, this.s.logmar_cols_raw, this.s.va_format
      ]), () => { this.debouncedCounts(); this._fpGen++; this.fprev.stale=true; this.debouncedFilteredPreview(); });
      // re-check the declared VA notation whenever the set of VA columns or the notation changes
      this.$watch(() => JSON.stringify([VA_ROLES.map(k=>this.s.roles[k]||""), this.s.logmar_cols_raw, this.s.va_format]),
                  () => this.debouncedVaDetect());
      // the logMAR column list follows the mapped VA roles until the user edits it
      this.$watch(() => JSON.stringify([this.s.do_logmar, VA_ROLES.map(k=>this.s.roles[k]||"")]),
                  () => this.seedLogmarCols());
      // pre-fill the cylinder sets once the pre and post sph/cyl/axis roles are mapped
      this.$watch(() => JSON.stringify(STD_ROLE_SETS.flat().map(k=>this.s.roles[k]||"")),
                  () => this.seedStdTriples());
      this.$watch(() => this.s.group_source_var, () => this.onGroupSourceChange());
      this.$watch(() => this.s.subset_col_raw, () => this.onSubsetColChange());
      // keep order strips + dichotomous GUI in sync with type selections
      this.$watch(() => JSON.stringify(SECTIONS.map(sec => TYPES.map(ty => this.s[sec][ty]))),
                  () => { this.syncOrders(); this.ensureDichoDefaults(); this.pruneWithinLevels(); });
      // matching preview goes stale whenever anything upstream of MatchIt changes
      // (not figure colours / labels); auto-refresh (debounced) while the Matching tab is open
      this.$watch(() => JSON.stringify([
        this.s.psm_covariates_raw, this.s.psm_method, this.s.psm_distance, this.s.psm_ratio,
        this.s.psm_replace, this.s.psm_caliper, this.s.psm_std_caliper, this.s.psm_m_order, this.s.psm_seed,
        this.groupKey(), this.s.reference_uid, this.s.filters, this.s.subsets, this.s.subset_mode,
        this.s.exclude_params_raw, this.s.do_matching
      ]), () => { this._mpGen++; this.matchprev.stale = true; this.debouncedMatchPreview(); });
      this.$watch(() => this.tab, (t) => {
        if (t === "match" && this.matchprev.stale) this.debouncedMatchPreview();
        if (t === "filters" && this.fprev.stale) this.debouncedFilteredPreview();
      });
      this.syncOrders();
      this.seedLogmarCols();
    },

    // ---- R setup: first-run "where is R?" screen ----
    rSetupOpen:false, rInfo:{rscript:null,info:"",candidates:[]},
    rPathInput:"", rMsg:"", rMsgOk:false,
    async loadRSetup(openIfMissing){
      try{
        const r=await fetch("/api/r-setup"); const j=await r.json();
        this.rInfo=j;
        if(!j.rscript && openIfMissing!==false) this.rSetupOpen=true;
        if(j.rscript && !this.rPathInput) this.rPathInput=j.rscript;
      }catch(e){ /* server not up yet */ }
    },
    async useRscript(p){
      if(!p) return;
      this.rMsg="Checking…"; this.rMsgOk=false;
      try{
        const r=await fetch("/api/r-setup",{method:"POST",
          headers:{"Content-Type":"application/json"}, body:JSON.stringify({path:p})});
        const j=await r.json();
        this.rMsg=j.message||""; this.rMsgOk=!!j.ok;
        if(j.ok){ this.rPathInput=j.rscript; await this.loadRSetup(false); }
      }catch(e){ this.rMsg="Could not reach the app server."; this.rMsgOk=false; }
    },
    async pickRscript(){
      try{
        const r=await fetch("/api/pick-rscript",{method:"POST"}); const j=await r.json();
        if(j.path){ this.rPathInput=j.path; await this.useRscript(j.path); }
        else if(j.error) this.rMsg="Could not open the file picker: "+j.error;
      }catch(e){ this.rMsg="Could not open the file picker."; }
    },

    // ---- global status traffic-light ----
    loadingFile:false,
    serverVersion:null,     // build id (APP_VERSION), compared with UI_VERSION
    publicVersion:null,     // release number the website and citation use
    versionMismatch:false,
    UI_VERSION,
    // An unreachable server is not an outdated one: say so, and keep checking
    // until it answers (the page may open before the server is listening).
    async checkVersion(){
      clearTimeout(this._hcT);
      try{
        const r=await fetch("/api/health");
        // a server without the health route is older than this page
        if(r.status===404){ this.serverDown=false; this.versionMismatch=true; this.serverVersion="(old)"; return; }
        if(!r.ok) throw new Error("HTTP "+r.status);
        const j=await r.json();
        this.serverDown=false;
        this.serverVersion=j.version||"(old)";
        this.publicVersion=j.public_version||null;
        this.versionMismatch = (j.version!==UI_VERSION);
        this.rInfo.rscript = j.rscript;
        // a run started before this page was (re)loaded is still going
        this.serverBusy = !!j.running && !this.running;
        this.watchBusy();
        if(!j.rscript) this.loadRSetup(true);
      }catch(e){
        this.serverDown=true;
        this._hcT=setTimeout(()=>this.checkVersion(), 3000);
      }
    },
    // While a run started elsewhere is active, ask /api/health every 3 s and
    // clear the "still running" state as soon as that run has ended.
    watchBusy(){
      clearTimeout(this._busyT);
      if(!this.serverBusy || this.running) return;
      this._busyT=setTimeout(async ()=>{
        try{
          const r=await fetch("/api/health");
          const j=await r.json();
          if(r.ok && !j.running && !this.running && this.serverBusy){
            this.serverBusy=false;
            if(this._busy409){ this.runError=null; this._busy409=false; }
            this.progressMsg="The earlier analysis has finished — you can run now.";
          }
        }catch(e){ /* server unreachable: keep the state, try again */ }
        this.watchBusy();
      }, 3000);
    },
    // header text: "v1.0.0 (build 2026-…)"
    versionLabel(){
      if(!this.serverVersion) return "v…";
      return this.publicVersion ? ("v"+this.publicVersion+" (build "+this.serverVersion+")") : ("build "+this.serverVersion);
    },
    appStatus(){
      if(this.serverDown) return {color:"red",
        label:"Cannot reach the StandardEyes server — is its console window still open? Retrying…"};
      if(this.versionMismatch) return {color:"red",
        label:"Server is running OLD code — close the app's console window and double-click 'Start StandardEyes.bat' again"};
      if(!this.rInfo.rscript) return {color:"red", label:"R is not set up yet — click ⚙ R setup"};
      if(this.runError) return {color:"red", label:"Run failed — see Run log"};
      if(this.countsError) return {color:"red", label:"Preview error"};
      if(this.filteredExport.error) return {color:"red", label:"Export failed"};
      if(this.loadingFile) return {color:"yellow", label:"Loading data file…"};
      if(this.cancelling) return {color:"yellow", label:"Stopping the analysis…"};
      if(this.running) return {color:"yellow", label:"Analysis running… "+Math.round(this.progress*100)+"%"};
      if(this.serverBusy) return {color:"yellow", label:"An analysis started earlier is still running"};
      if(this.exporting) return {color:"yellow", label:"Saving files…"};
      if(this.filteredExport.busy) return {color:"yellow", label:"Exporting filtered data…"};
      if(this.matchprev.busy) return {color:"yellow", label:"Previewing matching…"};
      if(this.countsBusy || this.hist.busy) return {color:"yellow", label:"Updating preview…"};
      if(!this.file) return {color:"red", label:"No data file loaded"};
      if(this.results) return {color:"green", label:"Run complete — results ready"};
      return {color:"green", label:"Ready — "+this.nrows.toLocaleString()+" rows loaded"};
    },

    // ================= helpers =================
    has(arr,v){ return Array.isArray(arr) && arr.includes(v); },
    toggle(arr,v){ const i=arr.indexOf(v); i>-1 ? arr.splice(i,1) : arr.push(v); },
    // the server trims the search text; so does the local filter
    filterOpts(list,q){ q=String(q||"").trim().toLowerCase(); return list.filter(c=>String(c).toLowerCase().includes(q)); },
    groupNames(){ return this.s.groups.map(g=>g.name).filter(n=>n); },
    // the group fields that change counts, filtering or matching (watchers)
    groupKey(){ return this.s.groups.map(g=>[g.name, g.values]); },
    // groups that reach the engine (a name and at least one value)
    usableGroupNames(){ return this.s.groups.filter(g=>g.name && g.values.length).map(g=>g.name); },
    // ---- the reference group: kept by identity (s.reference_uid) ----
    refGroup(){ return this.s.groups.find(g=>g._uid===this.s.reference_uid) || null; },
    refName(){ const g=this.refGroup(); return g ? String(g.name||"") : ""; },
    isRef(g){ return !!g && g._uid===this.s.reference_uid; },
    setReference(g){ if(g){ this.s.reference_uid=g._uid; this.s.reference_orphan=""; } },
    // a name edit never moves the reference: it stays on its row
    renameGroup(g, name){ g.name=name; },
    // why the chosen reference cannot be used ("" when it can)
    referenceProblem(){
      const g=this.refGroup();
      if(!g){
        if(this.s.reference_orphan)
          return "The reference group “"+this.s.reference_orphan+"” no longer exists — tick “reference” on one group.";
        return "No reference group is chosen — tick “reference” on one group.";
      }
      const ref=String(g.name||"");
      if(!ref.trim()) return "The reference group has no name — name it or choose another reference.";
      if(this.dupGroupNames().includes(ref.trim())) return "The reference group’s name “"+ref+"” is used by another group too — rename one of them.";
      if(!g.values.length) return "The reference group “"+ref+"” has no values yet — add its values or choose another reference.";
      return "";
    },
    // groups are keyed by name in the config: two rows with one name would
    // silently merge into one group
    dupGroupNames(){
      const L=this.s.groups.map(g=>String(g.name||"").trim()).filter(Boolean);
      return [...new Set(L.filter((n,i)=>L.indexOf(n)!==i))];
    },
    isDupGroup(g){ const n=String(g.name||"").trim(); return !!n && this.dupGroupNames().includes(n); },
    // first unused "<base><n>" among names
    nextName(base, names){
      let n=names.length+1;
      while(names.includes(base+n)) n++;
      return base+n;
    },
    // Subset names become run labels and file names (the engine sanitises
    // them like safeFilename): no duplicates, no reserved run names, nothing
    // that sanitises to the same file name. Only rows that reach the engine count.
    subsetNameProblems(){
      if(!this.s.subset_col_raw) return [];
      const out=[], seen={};
      for(const g of this.s.subsets){
        if(g.enabled===false || !g.values.length) continue;
        const name=String(g.name||"");
        const key=safeFilename(name).toLowerCase();
        if(!name.trim()) out.push("an enabled subset has no name");
        else if(RESERVED_RUN_NAMES.includes(key)) out.push("“"+name+"” is reserved for the combined run — rename the subset");
        else if(!key) out.push("“"+name+"” cannot be used as a file name — rename the subset");
        else if(seen[key]!==undefined) out.push(seen[key]===name
          ? "two subsets are named “"+name+"”"
          : "“"+seen[key]+"” and “"+name+"” would get the same file name");
        else seen[key]=name;
      }
      return out;
    },
    // the name the output files will carry, when the engine has to change it
    prefixOnDisk(){
      const p=this.s.output_prefix||"";
      const f=safeFilename(p) || "StandardEyes";
      return f!==p ? f : "";
    },

    // ---- does a mapped column exist in the loaded file? ----
    // Exactly the engine's resolver (resolveColumn = R .resolve_one): a name
    // that is missing or ambiguous there is missing here too.
    colResolves(name){
      if(!name || !this.cols.length) return true;
      return resolveColumn(this.cols, name).col!==null;
    },
    roleMissing(k){ const v=this.s.roles[k]; return !!v && !this.colResolves(v); },
    // every column a setting refers to that is not in this file (a preset from
    // another study, a renamed export); the engine drops such settings and the
    // matching rows / tables / figures come out blank
    unresolvedMappings(){
      if(!this.file || !this.cols.length) return [];
      const s=this.s, names=new Set(this.relevantCols());
      for(const v of Object.values(s.roles)) if(v) names.add(v);
      for(const v of [...s.regression_covariates_raw, ...s.patient_level_params_raw]) if(v) names.add(v);
      return [...names].filter(c=>typeof c==="string" && !this.colResolves(c));
    },
    // headers that the case/punctuation-insensitive fallback cannot tell apart:
    // they must be mapped by their exact name. The list /api/load returns is
    // used when there is one; otherwise the same grouping by the engine's key.
    similarHeaders(){
      if(Array.isArray(this.similarHdrs)) return this.similarHdrs;
      const by={};
      for(const c of this.cols){ const k=colKey(c); if(k) (by[k]=by[k]||[]).push(c); }
      return Object.values(by).filter(a=>a.length>1);
    },

    typingFields(){
      const out=[];
      for(const sec of SECTIONS) for(const ty of TYPES)
        out.push({label: SECTION_TITLES[sec]+" · "+ty, arr: this.s[sec][ty], key: sec+"_"+ty, sec, ty});
      return out;
    },
    // a variable has ONE statistical type per section: ticking it under one
    // type removes it from the other three (R would otherwise pick by list order)
    toggleType(sec, ty, v){
      const arr=this.s[sec][ty], i=arr.indexOf(v);
      if(i>-1){ arr.splice(i,1); return; }
      for(const other of TYPES) if(other!==ty){
        const o=this.s[sec][other], j=o.indexOf(v);
        if(j>-1) o.splice(j,1);
      }
      arr.push(v);
    },
    // variables that a preset / older config lists under two types of one section
    typeConflicts(){
      const out=[];
      for(const sec of [...SECTIONS, "within_group"]){
        const seen={};
        for(const ty of TYPES) for(const v of this.s[sec][ty]) (seen[v]=seen[v]||[]).push(ty);
        for(const [v,tys] of Object.entries(seen)) if(tys.length>1)
          out.push({sec, v, types:tys});
      }
      return out;
    },
    colMultis(){
      return [
        {label:"Summarize per PATIENT (not per eye)", arr:this.s.patient_level_params_raw, key:"plvl"},
        {label:"Exclude rows missing these columns",  arr:this.s.exclude_params_raw, key:"excl"},
      ];
    },

    // ---- variable output order (drag & drop) ----
    selectedInSection(sec){
      const seen=new Set(); const out=[];
      for(const ty of TYPES) for(const v of this.s[sec][ty]) if(!seen.has(v)){ seen.add(v); out.push(v); }
      return out;
    },
    syncOrders(){
      // Dedupe selection lists (duplicate x-for keys break Alpine) — but only
      // WRITE when content actually changed: this method runs from a watcher
      // that tracks these very arrays, and Alpine fires watchers on every
      // write (not just value changes), so unconditional writes loop forever.
      const dedup = (a)=>{ const u=[...new Set(a)]; return u.length===a.length ? null : u; };
      for(const sec of [...SECTIONS, "within_group"])
        for(const ty of TYPES){
          const u=dedup(this.s[sec][ty]);
          if(u) this.s[sec][ty]=u;
        }
      for(const sec of SECTIONS){
        const sel=this.selectedInSection(sec);
        const cur=this.s[sec].order||[];
        const next=[...new Set(cur.filter(v=>sel.includes(v)).concat(sel.filter(v=>!cur.includes(v))))];
        if(JSON.stringify(next)!==JSON.stringify(cur)) this.s[sec].order=next;
      }
    },
    dropChip(sec,i){
      const d=this.drag;
      if(d.sec!==sec || d.i===null || d.i===i) { this.drag={sec:null,i:null}; return; }
      const arr=this.s[sec].order;
      const [moved]=arr.splice(d.i,1);
      arr.splice(i,0,moved);
      this.drag={sec:null,i:null};
    },
    // a drag ended outside every row leaves no row marked as dragged
    endDrag(){ this.drag={sec:null,i:null}; },
    startDrag(e, sec, i, v){
      // Firefox starts a drag only when data is set
      if(e && e.dataTransfer){ try{ e.dataTransfer.setData("text/plain", String(v)); e.dataTransfer.effectAllowed="move"; }catch(_){} }
      this.drag={sec, i};
    },
    moveVar(sec,i,d){
      this.endDrag();
      const arr=this.s[sec].order, j=i+d;
      if(j<0 || j>=arr.length) return;
      [arr[i],arr[j]]=[arr[j],arr[i]];
    },
    varType(sec,v){
      for(const ty of TYPES) if(this.s[sec][ty].includes(v))
        return ty.charAt(0).toUpperCase()+ty.slice(1);
      return '';
    },

    // ---- dichotomous positive-level GUI ----
    dichoVars(){
      const seen=new Set(); const out=[];
      for(const sec of SECTIONS) for(const v of this.s[sec].dichotomous) if(!seen.has(v)){ seen.add(v); out.push(v); }
      return out;
    },
    async ensureDichoDefaults(){
      if(!this.file) return;
      // a reply for a config or sheet that has since been replaced is dropped
      const s=this.s, gen=this._fileGen;
      for(const v of this.dichoVars()){
        const vals = await this.fetchDistinct(v);
        if(this.s!==s || this._fileGen!==gen) return;
        let cur = s.dicho_map[v];
        let absent = false;
        if(cur!==undefined && cur!==null && cur!=="" && vals.length && !this.distinctTrunc[v]){
          // the same level spelled differently ("1.0" / "1", "False" / "FALSE",
          // " yes " / "Yes") is the data's level, exactly as R resolves it
          const hit=resolveLevel(cur, vals);
          if(hit!==null){ if(hit!==String(cur)) s.dicho_map[v]=hit; cur=hit; delete this.dichoUnseen[v]; }
          else if(vals.length===1){
            // a one-valued column: the preferred event simply does not occur
            // here; keep it, so the row reads "<level> 0/N" as R reports it
            this.dichoUnseen[v]=String(cur);
          }
          // a level this file does not contain (other coding, a typo) would
          // make R fall back to its own default: guess again and show what was replaced
          else { absent=true; this.dichoReplaced[v]=String(cur); delete this.dichoUnseen[v]; }
        }
        if(cur===undefined || cur===null || cur==="" || absent){
          // 1) a known event coding; 2) with two levels, the complement of a
          // known non-event coding; 3) otherwise the second sorted value,
          // flagged in the GUI so the user checks it
          let guess = DICHO_PREFS.find(p=>vals.includes(p)), src="pref";
          if(guess===undefined && vals.length===2){
            const neg=vals.filter(x=>DICHO_NEG.includes(x));
            if(neg.length===1){ guess=vals.find(x=>x!==neg[0]); src="complement"; }
          }
          if(guess===undefined){ guess = vals.length>=2 ? vals[1] : (vals[0]||""); src="fallback"; }
          s.dicho_map[v]=guess;
          this.dichoGuessSrc[v]=src;
        }
      }
      // drop stale entries
      const cur=this.dichoVars();
      for(const k of Object.keys(s.dicho_map)) if(!cur.includes(k)) delete s.dicho_map[k];
      for(const k of Object.keys(this.dichoGuessSrc)) if(!cur.includes(k)) delete this.dichoGuessSrc[k];
      for(const k of Object.keys(this.dichoReplaced)) if(!cur.includes(k)) delete this.dichoReplaced[k];
      for(const k of Object.keys(this.dichoUnseen)) if(!cur.includes(k)) delete this.dichoUnseen[k];
    },
    // the column's values, plus a kept preset level that does not occur in it
    dichoOptions(v){
      const L=this.distinctCache[v]||[], c=this.s.dicho_map[v];
      return (c!==undefined && c!==null && c!=="" && !L.includes(c)) ? [...L, c] : L;
    },
    setDichoLevel(v,val){ this.s.dicho_map[v]=val; this.dichoGuessSrc[v]="user"; delete this.dichoReplaced[v]; delete this.dichoUnseen[v]; },
    // amber cue when the default level was a blind alphabetical pick
    dichoFallback(v){ return this.dichoGuessSrc[v]==="fallback" && !!this.s.dicho_map[v]; },
    dichoLevelCount(v){ return (this.distinctCache[v]||[]).length; },

    // ---- live "what will I get?" panel preview ----------------------------
    // MIRRORS the gating in R/graphs.R. If that gating changes, change this too;
    // the run itself is always authoritative.
    // a visit counts only when its columns are in the loaded file (pipeline.R
    // skips a visit whose mapped column it cannot find)
    stabilityReady(){
      const C=c=>!!c && this.colResolves(c);
      return !!this.s.stability_figs &&
             this.s.stability_visits.some(v=>C(v.time) && (C(v.seq) || (C(v.sph) && C(v.cyl))));
    },
    // a role counts only when its column is in the loaded file (the engine
    // drops a role whose column it cannot find)
    roleMapped(k){ const v=this.s.roles && this.s.roles[k]; return !!v && this.colResolves(v); },
    panelPlan(){
      const s=this.s, R=k=>this.roleMapped(k);
      const pu=R('post_ucva'), pcv=R('pre_bcva'), pov=R('post_bcva');
      const pseq=R('pre_seq'), poseq=R('post_seq');
      const pcyl=R('pre_cyl'), pax=R('pre_axis'), ocyl=R('post_cyl'), oax=R('post_axis');
      const av=pcyl&&pax&&ocyl&&oax;
      const four=(s.graph_set==='four'), six=(s.graph_set==='six');
      const astig=!!s.astig_panels;
      const stab=this.stabilityReady() && !four && !six;
      const out=[]; const add=(c,n)=>{ if(c) out.push(n); };
      // Vision and Efficacy share one CDVA reference (graphs.R cdva_ref):
      // preop CDVA for the corneal sets (nine / six), postop CDVA for four
      const cref = four ? pov : pcv;
      add(pu&&cref,'Vision');
      add(pu&&cref,'Efficacy');
      add(!four&&pcv&&pov,'Safety');
      add(poseq,'SEQ accuracy');
      add(!four&&pseq&&poseq,'Attempted vs achieved');
      add(!six&&!four&&poseq&&ocyl,'DEQ accuracy');
      add(stab,'Stability');
      add(astig&&pcyl&&ocyl,'Refractive astigmatism');
      add(astig&&!six&&!four&&av,'TIA vs SIA');
      // the correction index is drawn only as this histogram: a ratio has no
      // meridian, so it has no double-angle or semicircle plot (D3)
      add(astig&&!six&&!four&&av,'Correction index histogram');
      add(astig&&!six&&!four&&av,'Angle of error');
      return out;
    },
    panelNote(){
      const p=this.panelPlan();
      const miss=Object.keys(ROLE_LABELS).filter(k=>this.roleMissing(k));
      const missNote=miss.length ? ' Not in this file (ignored): '+miss.map(k=>ROLE_LABELS[k]).join(', ')+'.' : '';
      if(!p.length) return 'Nothing to draw yet — map the column roles at the bottom of this tab.'+missNote;
      // the engine builds the composite only from two or more panels
      let t=(p.length>=2)
        ? p.length+' panels in the composite: '+p.join(' · ')+'.'
        : '1 individual panel ('+p[0]+') — the composite needs 2 or more panels.';
      if(p.includes('Stability'))
        t+=' Stability is an extra panel — nothing is displaced.';
      // name only the target-dependent panels that are actually drawn
      const tp=[];
      if(p.includes('SEQ accuracy')) tp.push('SEQ accuracy');
      if(p.includes('Attempted vs achieved')) tp.push('attempted-vs-achieved');
      if(tp.length && !this.roleMapped('target_seq'))
        t+=' '+tp.join(' and ')+(tp.length>1 ? ' assume' : ' assumes')+' every eye was aimed at plano (map Target SEQ if not).';
      return t+missNote+this.vectorFigNote();
    },
    // the standalone double-angle / semicircle figures have their own switches
    // and do not depend on "Include astigmatism panels" (composite only)
    // Their panels (graphs.R astig_jcrs_vectors, D3): the double-angle set is
    // PRE, SIA and PE, plus POST when a mapped target is not plano (decided per
    // eye by the engine; without a target PE = POST); the semicircle set is the
    // Alpins TIA, SIA and DV, all at plus axes
    vectorFigNote(){
      const s=this.s, R=k=>this.roleMapped(k), C=c=>!!c && this.colResolves(c);
      const tgt=C(s.astig_target.cyl) && C(s.astig_target.axis);
      const want=[], names=[];
      if(s.double_angle_figs){
        names.push('double-angle');
        want.push('double-angle plots of preoperative astigmatism, SIA and the prediction error (PE)'+
          (tgt ? ', plus postoperative astigmatism if any target is not plano'
               : ' - with no target mapped, PE is the postoperative astigmatism'));
      }
      if(s.semicircle_figs){ names.push('semicircle'); want.push('semicircle plots of TIA, SIA and DV'); }
      if(!want.length) return '';
      // the engine builds the vectors from pre/post cylinder + axis; supplied
      // vector columns replace the computed ones but never stand in for them
      const av=R('pre_cyl')&&R('pre_axis')&&R('post_cyl')&&R('post_axis');
      const supplied=['tia','sia','dv','pe'].some(k=>C(s.astig_vector_cols[k+'_mag']) && C(s.astig_vector_cols[k+'_axis']));
      return av
        ? ' Separate figures (plus-cylinder format, with-the-rule on the left): '+want.join('; ')+
          (supplied ? ' (supplied vector columns used where mapped).' : '.')
        : ' No '+names.join(' / ')+' plots: map Pre/Post Cylinder and Axis'+(supplied ? ' (supplied vector columns alone are not enough).' : '.');
    },
    // the astigmatism sheets need the same four roles as the vector figures
    astigSheetNote(){
      const R=k=>this.roleMapped(k);
      if(!this.s.astig_sheet) return '';
      return (R('pre_cyl')&&R('pre_axis')&&R('post_cyl')&&R('post_axis'))
        ? 'Each run’s workbook gets the “Astigmatism (JCRS 2025)” and “Astigmatism vectors” sheets (and an Astig_M_ sheet per matched pairing), with a CSV copy beside it.'
        : 'No astigmatism sheets yet: map Pre/Post Cylinder and Axis under Column roles (bottom of this tab).';
    },
    // the figure width box: blank, or a width in cm above 0 and at most 60
    // (config_schema FIGURE_WIDTH_MAX_CM, graphs.R GR_WIDTH_CM_MAX)
    figureWidthProblem(){
      const v=String(this.s.figure_width_cm??"").trim();
      if(!v) return "";
      const n=Number(v);
      return (isFinite(n) && n>0 && n<=60) ? "" : "Enter a width above 0 and at most 60 cm, or leave the box empty for each figure's own size.";
    },

    // ---- within-group unit of analysis (per variable, any type) ----
    // Listed in type order (continuous, ordinal, dichotomous, categorical);
    // within-group variables have no output-order list of their own.
    withinVars(){
      const seen=new Set(); const out=[];
      for(const ty of TYPES) for(const v of this.s.within_group[ty]) if(!seen.has(v)){ seen.add(v); out.push(v); }
      return out;
    },
    pruneWithinLevels(){
      const cur=this.withinVars();
      for(const k of Object.keys(this.s.within_levels))
        if(!cur.includes(k)) delete this.s.within_levels[k];
    },

    // ================= file =================
    async pickFile(){
      try{
        const r = await fetch("/api/pick-file",{method:"POST"});
        const j = await r.json();
        if(r.ok && j.path){ this.pathInput = j.path; await this.loadFile(); }
        else if(!r.ok){ alert(j.detail || "Could not open file dialog. Type the path instead."); }
      }catch(e){ alert("File dialog error: "+e); }
    },
    async loadFile(){
      const path = this.pathInput.trim();
      if(!path){ alert("Choose or type a file path first."); return; }
      // the chosen sheet belongs to the workbook it was picked from: another
      // file opens on its own first sheet (or the sheet a pending preset names)
      const same = p => String(p||"").trim().replace(/^"+|"+$/g,"").replace(/\//g,"\\").toLowerCase();
      const sameFile = !!this.file && same(path)===same(this.file);
      const sheet = sameFile ? this.sheet : null;
      this.loadingFile=true;
      try{
        const r = await fetch("/api/load",{method:"POST",headers:{'Content-Type':'application/json'},
          body: JSON.stringify({path, sheet})});
        const j = await r.json().catch(()=>({detail:"Load failed (HTTP "+r.status+")"}));
        if(!r.ok){ alert(j.detail||"Load failed"); return; }
        this.file=j.path; this.sheets=j.sheets; this.sheet=j.sheet;
        if(j.sheet_fallback)
          alert("The sheet “"+j.sheet_fallback+"” is not in this workbook. Showing the sheet “"+j.sheet+"”.");
        // a preset loaded before this file names the sheet it was made for
        const want=this._pendingSheet; this._pendingSheet=null;
        if(want!==null && want!==undefined && want!==""){
          const name=this.resolveSheet(want, j.sheets);
          if(name && name!==j.sheet){ this.sheet=name; this.pathInput=j.path; return await this.loadFile(); }
          if(!name) alert("The study preset was made for the sheet “"+want+"”, which is not in this workbook. Showing the sheet “"+j.sheet+"” — check that it holds the same data.");
        }
        this._fileGen++;
        // the Filters and Matching previews showed the previous sheet / file:
        // drop them (and any reply still on its way) and fetch them again -
        // each one runs when its tab is open, or as soon as the tab is opened
        this.invalidateDataPreviews();
        this.cols=j.columns; this.nrows=j.n_rows; this.ncols=j.n_cols; this.preview=j.preview;
        this.renamedHeaders=Array.isArray(j.renamed_headers) ? j.renamed_headers : [];
        this.similarHdrs=Array.isArray(j.similar_headers) ? j.similar_headers : null;
        this.sortCol=""; this.sortDir="asc"; this.distinctCache={}; this.distinctTrunc={}; this.distinctQ={};
        if(!this.s.output_dir) this.s.output_dir = j.path.replace(/[\\/][^\\/]*$/,"") ;
        await this.onGroupSourceChange(); await this.onSubsetColChange();
        this.seedStdTriples();
        await this.ensureDichoDefaults();
        this.prefetchFilterDistincts();
        this.updateCounts();
        this.detectVa();
      }catch(e){ alert("Load error: "+e); }
      finally{ this.loadingFile=false; }
    },
    invalidateDataPreviews(){
      this._fpSeq++; this._fpGen++;
      this.fprev.data=null; this.fprev.error=null; this.fprev.busy=false; this.fprev.stale=true;
      this._mpSeq++; this._mpGen++;
      this.matchprev.data=null; this.matchprev.error=null; this.matchprev.busy=false;
      this.matchprev.reason=""; this.matchprev.stale=true; this.matchprev.run="";
      this.debouncedFilteredPreview(); this.debouncedMatchPreview();
    },
    // Values of one column of the loaded sheet. A reply that arrives after
    // another sheet / file was bound is returned to its caller (which checks
    // _fileGen itself) but never written into the new sheet's cache.
    async fetchDistinct(col){
      if(!col) return [];
      if(this.distinctCache[col]) return this.distinctCache[col];
      const gen=this._fileGen;
      let vals=[], trunc=false;
      try{
        const r = await fetch("/api/distinct?col="+encodeURIComponent(col));
        const j = await r.json();
        vals = (r.ok!==false && Array.isArray(j.values)) ? j.values : [];
        trunc = !!j.truncated;
      }catch(e){ vals=[]; trunc=false; }
      if(gen!==this._fileGen) return vals;
      // a concurrent request for the same column may have filled it already
      if(!this.distinctCache[col]){ this.distinctCache[col]=vals; this.distinctTrunc[col]=trunc; }
      return this.distinctCache[col];
    },
    // A column with more distinct values than the server lists (1000) is
    // searched on the server: the box's text goes to /api/distinct?q=, so a
    // value beyond the first 1000 can still be found and ticked.
    searchDistinct(col, q){
      if(!col || !this.distinctTrunc[col]) return;
      clearTimeout(this._dqt);
      q=String(q||"").trim();
      if(!q){ delete this.distinctQ[col]; return; }
      this._dqt=setTimeout(async ()=>{
        const seq=++this._dqSeq, gen=this._fileGen;
        try{
          const r=await fetch("/api/distinct?col="+encodeURIComponent(col)+"&q="+encodeURIComponent(q));
          const j=await r.json();
          if(seq===this._dqSeq && gen===this._fileGen && r.ok) this.distinctQ[col]={q, values:j.values||[], truncated:!!j.truncated};
        }catch(e){}
      },300);
    },
    // options of a value picker for column col, narrowed by the search text q
    valueOpts(col, q, list){
      const base=list || this.distinctCache[col] || [];
      const hit=this.distinctQ[col];
      if(this.distinctTrunc[col] && hit && q && hit.q===String(q).trim()){
        const merged=[...new Set([...hit.values, ...base])];
        return this.filterOpts(merged, q);
      }
      return this.filterOpts(base, q);
    },
    truncNote(col){
      return this.distinctTrunc[col]
        ? "Only the first 1000 values are listed — type in the search box to search the whole column."
        : "";
    },
    // a reply is used only while its column (and sheet) is still the chosen
    // one: a slow reply for a column the user has since replaced is dropped
    async onGroupSourceChange(){
      const s=this.s, col=s.group_source_var, gen=this._fileGen;
      const vals=await this.fetchDistinct(col);
      if(this.s!==s || s.group_source_var!==col || this._fileGen!==gen) return;
      this.groupSourceValues=vals;
    },
    async onSubsetColChange(){
      const s=this.s, col=s.subset_col_raw, gen=this._fileGen;
      const vals=await this.fetchDistinct(col);
      if(this.s!==s || s.subset_col_raw!==col || this._fileGen!==gen) return;
      this.subsetColValues=vals; this.seedTreatmentSubsets();
    },
    // the user picking a subset column by hand (combo) may seed default
    // subsets again; the programmatic $watch above must not clear the flag
    setSubsetCol(v){ this._subsetsFromCfg=false; this.s.subset_col_raw=v; },
    // a subset column that is not in the loaded file would make R stop (or,
    // in older engines, analyse every row under each subset's name)
    subsetColMissing(){
      const c=this.s.subset_col_raw;
      return !!(c && this.cols.length && !this.colResolves(c)) ||
             !!(this.counts && this.counts.subset_col_missing);
    },

    // ---- visual-acuity notation check (/api/va-detect) ----
    vaColumns(){
      const out=[];
      for(const k of VA_ROLES){ const c=this.s.roles[k]; if(c && !out.includes(c)) out.push(c); }
      for(const c of this.s.logmar_cols_raw) if(c && !out.includes(c)) out.push(c);
      return out;
    },
    debouncedVaDetect(){ clearTimeout(this._vat); this._vat=setTimeout(()=>this.detectVa(),500); },
    async detectVa(){
      // names the engine resolves (case / punctuation variants too); the server resolves them the same way
      const columns=this.vaColumns().filter(c=>this.colResolves(c));
      const seq=++this._vaSeq;
      if(!this.file || !columns.length){ this.vaGuess={}; this.vaWarn=[]; return; }
      try{
        // with the declared notation the server also says when the values
        // contradict it (e.g. ETDRS letters declared, decimal acuity recorded)
        const r=await fetch("/api/va-detect",{method:"POST",headers:{'Content-Type':'application/json'},
          body:JSON.stringify({columns, va_format:this.s.va_format})});
        const j=await r.json().catch(()=>({}));
        if(seq!==this._vaSeq) return;
        this.vaGuess = (r.ok && j.guess) ? j.guess : {};
        const w = r.ok ? j.warnings : null;
        this.vaWarn = Array.isArray(w) ? w.filter(Boolean).map(String)
                    : (w && typeof w==="object") ? Object.values(w).filter(Boolean).map(String) : [];
      }catch(e){ if(seq===this._vaSeq){ this.vaGuess={}; this.vaWarn=[]; } }
    },
    vaFormatLabel(f){ const e=VA_FORMATS.find(x=>x[0]===f); return e ? e[1] : String(f); },
    // columns whose values look like a different notation than the one declared
    vaMismatch(){
      return Object.entries(this.vaGuess||{})
        .filter(([c,g])=>g && g!==this.s.va_format && this.vaColumns().includes(c))
        .map(([c,g])=>({col:c, guess:g}));
    },

    // ---- relevant-columns projection for all preview tables ----
    relevantOnly: true,
    relevantCols(){
      const s=this.s; const out=[];
      const add=(...vs)=>{ for(const v of vs.flat(2)) if(v && typeof v==="string" && !out.includes(v)) out.push(v); };
      add(s.patient_id_raw, s.eye_id_raw, s.group_source_var, s.subset_col_raw);
      add(s.filters.map(f=>f.var));
      add(s.exclude_params_raw);
      for(const sec of SECTIONS) add(s[sec].order);
      add(TYPES.map(t=>s.within_group[t]));
      add(s.psm_covariates_raw, s.gee_adjust_covariates_raw);
      add(s.age_var, s.fu_var, s.sex_var);
      if(s.do_logmar) add(s.logmar_cols_raw);
      if(s.do_standardize_minus_cyl) add(s.std_triples.flatMap(t=>[t.sph,t.cyl,t.axis]));
      if(s.stability_figs) add(s.stability_visits.flatMap(v=>[v.time,v.seq,v.sph,v.cyl]));
      add(Object.values(s.astig_vector_cols)); add(Object.values(s.astig_target));
      return out;
    },
    // project a {columns, rows} table onto the relevant columns (kept in
    // config order); synthetic matching columns always stay in front
    displayTable(tbl){
      if(!tbl || !tbl.columns || !this.relevantOnly) return tbl;
      const KEEP=["Pairing","GroupLabel","MatchWeight","PropensityScore"].filter(c=>tbl.columns.includes(c));
      const rel=this.relevantCols().filter(c=>tbl.columns.includes(c) && !KEEP.includes(c));
      if(rel.length===0) return tbl;   // nothing configured yet -> show everything
      const names=[...KEEP, ...rel];
      const idx=names.map(c=>tbl.columns.indexOf(c));
      return {...tbl, columns:names, rows:(tbl.rows||[]).map(r=>idx.map(i=>r[i]))};
    },

    // ---- sortable data preview ----
    async sortBy(col){
      if(this.sortCol===col){ this.sortDir = this.sortDir==="asc" ? "desc" : "asc"; }
      else { this.sortCol=col; this.sortDir="asc"; }
      this.showHist(col);   // clicking a header also charts that column
      const seq=++this._prevSeq;
      try{
        const r = await fetch("/api/preview?sort="+encodeURIComponent(this.sortCol)+"&dir="+this.sortDir);
        const j = await r.json();
        if(seq===this._prevSeq && r.ok) this.preview=j;
      }catch(e){}
    },
    sortMark(col){ return this.sortCol===col ? (this.sortDir==="asc"?" ▲":" ▼") : ""; },

    // ---- live column histogram / within-group analysis zone ----
    async showHist(col){
      col = col || this.hist.col;
      if(!col || !this.file) return;
      this.hist.col=col; this.hist.busy=true; this.hist.error=null;
      const seq=++this._histSeq;
      const body = {col, cfg:this.buildCfg(), stage:this.hist.stage,
                    group:this.hist.group || null,
                    matched_file:(this.hist.stage==="matched" && this.hist.mfile) ? this.hist.mfile : null};
      try{
        const r = await fetch("/api/histogram",{method:"POST",headers:{'Content-Type':'application/json'},
          body: JSON.stringify(body)});
        const j = await r.json().catch(()=>({detail:r.statusText}));
        if(seq!==this._histSeq) return;   // a newer request owns the panel
        if(r.ok) this.hist.data=j;
        else { this.hist.data=null; this.hist.error=j.detail||"histogram failed"; }
      }catch(e){ if(seq!==this._histSeq) return; this.hist.data=null; this.hist.error=String(e); }
      this.hist.busy=false;
    },
    // filters changed -> re-sync (the matched cohort is fixed, so skip there)
    refreshHist(){ if(this.hist.col && this.hist.stage==="filtered") this.showHist(); },
    matchedAvailable(){ return !!(this.results && this.results.matched_files && this.results.matched_files.length); },
    matchedPaths(){ return ((this.results && this.results.matched_files) || []).map(f=>f.path); },
    // every run writes new matched files: a path from an earlier run is
    // rejected by the server, so always point at one of the CURRENT files
    syncHistMatched(){
      const paths=this.matchedPaths();
      if(!paths.includes(this.hist.mfile)) this.hist.mfile=paths[0]||"";
      if(this.hist.stage==="matched" && !paths.length) this.hist.stage="filtered";
    },
    setHistStage(st){
      if(st==="matched" && !this.matchedAvailable()) return;
      this.hist.stage=st;
      this.syncHistMatched();
      if(this.hist.col) this.showHist();
    },
    histAllLabel(){
      // an eye matched in several pairings is counted once (the server
      // de-duplicates the stacked Matched_Data rows)
      if(this.hist.stage==="matched") return "matched cohort, distinct eyes";
      return this.hist.group ? "filtered cohort" : "all rows";
    },
    histKeptLabel(){
      if(!this.hist.group) return "kept by filters";
      return this.hist.stage==="matched" ? this.hist.group+", distinct eyes" : this.hist.group;
    },
    histMax(){
      const b=(this.hist.data&&this.hist.data.bins)||[];
      return Math.max(1, ...b.map(x=>x.all||0));
    },
    barW(n){ return (100*(n||0)/this.histMax()).toFixed(2)+"%"; },
    fmtStat(x){ return (x===null||x===undefined) ? "—" : (Math.abs(x)>=100 ? x.toFixed(1) : x.toFixed(2)); },

    // ================= repeatable rows =================
    groupColor(i){ const g=this.s.groups[i]; return (g && g.color) || DEFAULT_GROUP_COLORS[i % DEFAULT_GROUP_COLORS.length]; },
    addGroup(){
      const n=this.s.groups.length+1;
      this.s.groups.push({name:this.nextName("Group", this.s.groups.map(g=>g.name)), values:[],
                          color:DEFAULT_GROUP_COLORS[(n-1)%DEFAULT_GROUP_COLORS.length], _uid:++GROUP_UID});
    },
    // removing the reference row clears the reference (the user picks the next
    // one, see referenceProblem); it never jumps to another group unseen
    removeGroup(i){
      const [gone]=this.s.groups.splice(i,1);
      if(gone && gone._uid===this.s.reference_uid){ this.s.reference_uid=null; this.s.reference_orphan=""; }
    },
    addFilter(){ this.s.filters.push(withUid({var:"", op:"in", value:[]})); },
    removeFilter(i){ this.s.filters.splice(i,1); },
    // a rule reaches the engine only with a column and an operator; buildCfg
    // and the per-rule counts (stepIndex) must use this same test
    ruleSent(f){ return !!(f && f.var && f.op); },
    // numeric operators need a number: say so before the run instead of letting
    // the rule silently keep (or drop) everything
    filterValueProblem(f){
      if(!this.ruleSent(f)) return "";
      const num=x=>x!==null && x!==undefined && String(x).trim()!=="" && isFinite(Number(String(x).trim()));
      const k=this.opKind(f.op);
      if(k==="text" && !["contains","not_contains"].includes(f.op) && !num(f.value))
        return "'"+f.op+"' compares numbers — enter a number, or use in / notin / contains for text";
      if(k==="range" && Array.isArray(f.value) && !(num(f.value[0]) && num(f.value[1])))
        return "enter both a lower and an upper number";
      // an empty pattern would keep only the rows where the column is blank
      // ('contains') or change nothing ('not_contains'): the server refuses it too
      if(["contains","not_contains"].includes(f.op) &&
         !(Array.isArray(f.value) ? f.value : [f.value]).some(x=>x!==null && x!==undefined && String(x).trim()!==""))
        return "enter the text for '"+f.op+"'";
      return "";
    },

    // ---- op-aware filter value editing ----
    // none: isna/notna | range: between/abs_between (two cells)
    // multi: in/notin (searchable checkbox list of the column's values)
    // text: everything else (single number or contains pattern)
    opKind(op){
      if(op==="isna"||op==="notna") return "none";
      if(op==="between"||op==="abs_between") return "range";
      if(op==="in"||op==="notin") return "multi";
      return "text";
    },
    filterPlaceholder(f){
      return ["contains","not_contains"].includes(f.op) ? "text pattern, e.g. PRK" : "numeric value, e.g. 18";
    },
    // badge text + tooltip of a rule the live preview could not apply
    // (column problems: "not found" / "ambiguous"; value problems: a number or
    // limit still to fill in = "incomplete", anything else = "invalid value")
    stepSkipLabel(st){
      // the server names the kind of every skipped step (preview.py
      // apply_filter_rules); the reason text is the fallback for older replies
      const SKIP_KIND={missing_column:"not found", ambiguous_column:"ambiguous",
                       incomplete:"incomplete", invalid:"invalid value"};
      if(st && SKIP_KIND[st.kind]) return SKIP_KIND[st.kind];
      const r=String((st && st.reason) || "");
      if(!r || r==="column not found") return "not found";
      if(/ambiguous/i.test(r)) return "ambiguous";
      return /needs (both|exactly)|no values selected|incomplete/i.test(r) ? "incomplete" : "invalid value";
    },
    stepSkipTitle(st){
      if(!st || !st.reason || st.reason==="column not found") return "column not found in this file — the rule is not applied";
      if(/ambiguous/i.test(st.reason)) return st.reason+" — pick the exact header (or rename one in the file); the rule is not applied yet";
      return st.reason+" — the rule is not applied yet";
    },
    coerceFilterValue(f){
      const k=this.opKind(f.op);
      if(k==="range"){
        if(!Array.isArray(f.value)) f.value=String(f.value||"").split(",").map(s=>s.trim());
        while(f.value.length<2) f.value.push("");
        f.value=f.value.slice(0,2);
      } else if(k==="multi"){
        if(!Array.isArray(f.value)) f.value=String(f.value||"").split(",").map(s=>s.trim()).filter(Boolean);
        if(f.var) this.fetchDistinct(f.var);
      } else if(k==="none"){ f.value=null; }
      else { if(Array.isArray(f.value)) f.value=f.value.join(", "); if(f.value==null) f.value=""; }
    },
    setFilterVar(f,v){ f.var=v; if(this.opKind(f.op)==="multi" && v) this.fetchDistinct(v); },
    prefetchFilterDistincts(){
      for(const f of this.s.filters)
        if(this.opKind(f.op)==="multi" && f.var) this.fetchDistinct(f.var);
    },
    addSubset(){ this.s.subsets.push(withUid({name:this.nextName("Subset", this.s.subsets.map(g=>g.name)), values:[], enabled:true})); },
    removeSubset(i){ this.s.subsets.splice(i,1); },
    // seed LASIK / PRK subsets from the actual treatment-type values, only for
    // a subset column the user picked by hand while none are defined. A loaded
    // preset / config is authoritative even when it deliberately has no subsets.
    seedTreatmentSubsets(){
      if(this._subsetsFromCfg || this.s.subsets.length) return;
      const vals=this.subsetColValues||[];
      if(!vals.length) return;
      const lasik=vals.filter(v=>/lasik/i.test(v));
      const prk=vals.filter(v=>/prk|lasek/i.test(v));
      const rows=[];
      if(lasik.length) rows.push(withUid({name:"LASIK", values:lasik, enabled:true}));
      if(prk.length)   rows.push(withUid({name:"PRK",   values:prk,   enabled:true}));
      if(rows.length) this.s.subsets=rows;
    },

    // ---- covariate suggestion for the Regression tab ----
    regsuggest:{busy:false, list:null, error:null},
    async suggestCovariates(){
      if(!this.file){ alert("Load a data file first."); return; }
      this.regsuggest.busy=true; this.regsuggest.error=null;
      try{
        const r=await fetch("/api/suggest-covariates",{method:"POST",headers:{'Content-Type':'application/json'},
          body:JSON.stringify({cfg:this.buildCfg()})});
        const j=await r.json();
        if(r.ok) this.regsuggest.list=j.covariates;
        else this.regsuggest.error=j.detail||"suggestion failed";
      }catch(e){ this.regsuggest.error=String(e); }
      this.regsuggest.busy=false;
    },
    useSuggested(){
      if(!this.regsuggest.list) return;
      this.s.regression_covariates_raw =
        this.regsuggest.list.filter(x=>x.suggested).map(x=>x.var);
    },

    // ---- live matching preview (real MatchIt on slim data) ----
    debouncedMatchPreview(){
      clearTimeout(this._mpt);
      this._mpt=setTimeout(()=>this.updateMatchPreview(),1200);
    },
    // why the preview cannot run with the current settings ("" = it can)
    matchPreviewBlocker(){
      if(!this.file) return "Load a data file first — the preview appears here.";
      if(!this.s.do_matching) return "Matching is off.";
      if(this.groupNames().length<2) return "Define at least two named groups (Data & Groups tab) — the preview appears here.";
      if(!this.s.psm_covariates_raw.length) return "Pick at least one matching covariate above — the preview appears here.";
      return "";
    },
    async updateMatchPreview(){
      if(this.tab!=="match") return;   // runs when the tab is opened (stale stays set)
      const why=this.matchPreviewBlocker();
      if(why){
        // the old table no longer describes these settings and nothing will
        // refresh it: clear it, drop any reply still on its way, and say why
        this._mpSeq++;
        this.matchprev.data=null; this.matchprev.error=null; this.matchprev.busy=false;
        this.matchprev.stale=false; this.matchprev.run=""; this.matchprev.reason=why;
        return;
      }
      this.matchprev.reason="";
      this.matchprev.busy=true; this.matchprev.error=null;
      const seq=++this._mpSeq, gen=this._mpGen;
      try{
        const r=await fetch("/api/match-preview",{method:"POST",headers:{'Content-Type':'application/json'},
          body:JSON.stringify({cfg:this.buildCfg()})});
        const j=await r.json().catch(()=>({detail:r.statusText}));
        if(seq!==this._mpSeq) return;   // superseded: the newer request finishes the job
        if(r.ok){
          // settings that changed after this request was sent keep the badge on
          this.matchprev.data=j; this.matchprev.stale=(gen!==this._mpGen);
          const rs=Object.keys(j.matched_preview||{});
          if(!rs.includes(this.matchprev.run)) this.matchprev.run=rs[0]||"";
        }
        else{
          // refused by the server: the old table does not describe these settings
          this.matchprev.error=j.detail||"preview failed";
          this.matchprev.data=null; this.matchprev.stale=false; this.matchprev.run="";
        }
      }catch(e){
        if(seq!==this._mpSeq) return;
        this.matchprev.error=String(e);
        this.matchprev.data=null; this.matchprev.stale=false; this.matchprev.run="";
      }
      this.matchprev.busy=false;
    },
    missingList(run, group){
      const mi=this.matchprev.data && this.matchprev.data.missing_info;
      if(!mi || !mi[run] || !mi[run][group]) return [];
      return Object.entries(mi[run][group].missing||{}).sort((a,b)=>b[1]-a[1]);
    },
    matchedRuns(){ return Object.keys((this.matchprev.data&&this.matchprev.data.matched_preview)||{}); },
    matchedTable(){
      const mp=(this.matchprev.data&&this.matchprev.data.matched_preview)||{};
      return mp[this.matchprev.run] || null;
    },

    // ---- live filtered-data preview (Filters tab) ----
    debouncedFilteredPreview(){
      clearTimeout(this._fpt);
      this._fpt=setTimeout(()=>this.updateFilteredPreview(),600);
    },
    async updateFilteredPreview(){
      if(this.tab!=="filters" || !this.file) return;
      this.fprev.busy=true; this.fprev.error=null;
      const seq=++this._fpSeq, gen=this._fpGen;
      try{
        const r=await fetch("/api/preview-filtered",{method:"POST",headers:{'Content-Type':'application/json'},
          body:JSON.stringify({cfg:this.buildCfg(), sort:this.fprev.sortCol, dir:this.fprev.sortDir})});
        const j=await r.json().catch(()=>({detail:r.statusText}));
        if(seq!==this._fpSeq) return;
        if(r.ok){ this.fprev.data=j; this.fprev.stale=(gen!==this._fpGen); }
        else this.fprev.error=j.detail||"preview failed";
      }catch(e){ if(seq!==this._fpSeq) return; this.fprev.error=String(e); }
      this.fprev.busy=false;
    },
    sortFiltered(col){
      if(this.fprev.sortCol===col){ this.fprev.sortDir=this.fprev.sortDir==="asc"?"desc":"asc"; }
      else { this.fprev.sortCol=col; this.fprev.sortDir="asc"; }
      this.updateFilteredPreview();
    },
    fSortMark(col){ return this.fprev.sortCol===col ? (this.fprev.sortDir==="asc"?" ▲":" ▼") : ""; },

    // ================= counts =================
    debouncedCounts(){ clearTimeout(this._ct); this._ct=setTimeout(()=>this.updateCounts(),400); },
    async updateCounts(){
      if(!this.file) return;
      this.countsBusy=true;
      const seq=++this._cntSeq;
      try{
        const r = await fetch("/api/preview-counts",{method:"POST",headers:{'Content-Type':'application/json'},
          body: JSON.stringify({cfg:this.buildCfg()})});
        const j = await r.json().catch(()=>({detail:r.statusText}));
        if(seq!==this._cntSeq) return;
        if(r.ok){ this.counts=j; this.countsError=null; }
        else { this.countsError = j.detail || "preview failed"; }
      }catch(e){ if(seq!==this._cntSeq) return; this.countsError=String(e); }
      this.countsBusy=false;
      this.refreshHist();   // keep the live histogram in sync with the filters
    },
    // UI row i -> index into counts.filter_steps (rules without a column are
    // not sent, so later rows shift); -1 when row i is not sent at all
    stepIndex(i){
      const f=this.s.filters[i];
      if(!this.ruleSent(f)) return -1;
      return this.s.filters.slice(0,i).filter(x=>this.ruleSent(x)).length;
    },
    filterStep(i){
      const k=this.stepIndex(i);
      return (k>=0 && this.counts && this.counts.filter_steps) ? (this.counts.filter_steps[k]||null) : null;
    },
    // per-group remaining / excluded at UI filter row i
    groupLoss(i){
      const c=this.counts, k=this.stepIndex(i);
      if(k<0 || !c || !c.filter_steps || !c.filter_steps[k] || !c.filter_steps[k].groups) return [];
      const cur=c.filter_steps[k].groups;
      const prev=(k===0) ? (c.groups_after_exclude||{}) : ((c.filter_steps[k-1] && c.filter_steps[k-1].groups)||{});
      return Object.keys(cur).map(g=>({g, remaining:cur[g], excluded:(prev[g]??cur[g])-cur[g]}));
    },

    // ================= cfg build / apply =================
    buildCfg(){
      const s=this.s;
      const groups={}; const group_colors={}; const group_labels={};
      s.groups.forEach((g,i)=>{ if(g.name && g.values.length){
        groups[g.name]=g.values.slice(); group_colors[g.name]=this.groupColor(i);
        if(g.fig_label) group_labels[g.name]=g.fig_label;
      } });
      // only enabled subset rows with values participate in the engine-facing
      // map; subset_rows keeps every row (incl. unticked ones) for the GUI
      const subsets={}; for(const g of s.subsets){ if(g.enabled!==false && g.name && g.values.length) subsets[g.name]=g.values.slice(); }
      const subset_rows = s.subsets.map(g=>({name:g.name, values:g.values.slice(), enabled:g.enabled!==false}));
      const subset_mode = ["separate","both","pooled"].includes(s.subset_mode) ? s.subset_mode : "separate";
      const secCfg = sec => ({
        continuous:s[sec].continuous, ordinal:s[sec].ordinal,
        dichotomous:s[sec].dichotomous, categorical:s[sec].categorical,
        order:s[sec].order,
      });
      const wg = {continuous:s.within_group.continuous, ordinal:s.within_group.ordinal,
                  dichotomous:s.within_group.dichotomous, categorical:s.within_group.categorical};
      return {
        sheet_name: this.sheet, guess_max: s.guess_max,
        patient_id_raw:s.patient_id_raw, eye_id_raw:s.eye_id_raw, group_source_var:s.group_source_var,
        groups, group_colors, group_labels,
        // only a group that reaches the engine can be the reference; previews
        // fall back to the first such group, while run() refuses to start until
        // the reference is valid (referenceProblem), so the run never switches silently
        reference_group: this.usableGroupNames().includes(this.refName()) ? this.refName()
                         : (this.usableGroupNames()[0]||this.groupNames()[0]||""),
        patient_level_params_raw:s.patient_level_params_raw, exclude_params_raw:s.exclude_params_raw,
        age_var:s.age_var, fu_var:s.fu_var, sex_var:s.sex_var,
        baseline:secCfg("baseline"), preop:secCfg("preop"), outcome:secCfg("outcome"),
        filter_rules: s.filters.filter(f=>this.ruleSent(f)).map(f=>({var:f.var, op:f.op, value:f.value})),
        do_matching:s.do_matching, psm_covariates_raw:s.psm_covariates_raw,
        psm_method:s.psm_method, psm_distance:s.psm_distance, psm_ratio:s.psm_ratio,
        psm_replace:s.psm_replace, psm_caliper:s.psm_caliper, psm_std_caliper:s.psm_std_caliper, psm_m_order:s.psm_m_order,
        psm_seed:(s.psm_seed===""||s.psm_seed===null||!isFinite(Number(s.psm_seed))) ? 20250601 : Math.trunc(Number(s.psm_seed)),
        run_gee_stage:s.run_gee_stage, run_main_gee:s.run_main_gee, run_adjusted_gee:s.run_adjusted_gee,
        gee_adjust_covariates_raw:s.gee_adjust_covariates_raw, use_matching_weights_in_gee:s.use_matching_weights_in_gee,
        add_gee_columns:s.add_gee_columns, gee_source_for_tables:s.gee_source_for_tables,
        gee_small_sample:s.gee_small_sample, table_tests:s.table_tests,
        subset_col_raw:s.subset_col_raw, subsets, subset_rows, subset_mode,
        subset_pooled: subset_mode!=="separate",   // legacy flag, kept for older readers
        within_group: wg,
        within_group_levels: Object.fromEntries(this.withinVars()
          .filter(v=>s.within_levels[v]==="patient").map(v=>[v,"patient"])),
        do_standardize_minus_cyl:s.do_standardize_minus_cyl,
        std_triples: s.std_triples.filter(t=>t.cyl).map(t=>({sph:t.sph, cyl:t.cyl, axis:t.axis})),
        // GUI-only: true once the set list was chosen (filled in, edited or
        // emptied by the user); an empty list without it was simply never seeded
        std_triples_user: !!(this._stdNoSeed || s.std_triples.length),
        va_format:s.va_format,
        do_logmar:s.do_logmar, logmar_cols_raw:s.logmar_cols_raw,
        // GUI-only: true once the logMAR list was chosen; without it the list
        // keeps following the mapped VA roles after a preset is reloaded
        logmar_cols_user: !!this._logmarUser,
        make_meyestro:s.make_meyestro, make_alpins:s.make_alpins, make_pub_table:s.make_pub_table,
        make_graphs:s.make_graphs, graph_set:s.graph_set, graph_formats:s.graph_formats.slice(),
        graph_dpi:s.graph_dpi,
        figure_style:s.figure_style, p_style:s.p_style,
        // blank = each figure's own size (the engine and the server read null so)
        figure_width_cm:(()=>{ const w=String(s.figure_width_cm??"").trim();
          return !w ? null : (isFinite(Number(w)) ? Number(w) : w); })(),
        stability_figs:s.stability_figs,
        // a visit needs a time column plus either a ready SEQ or a full sph+cyl pair
        stability_visits: s.stability_visits
          .filter(v=>v.time && (v.seq || (v.sph && v.cyl)))
          .map(v=>({label:String(v.label||""), time:v.time, seq:v.seq||"", sph:v.sph||"", cyl:v.cyl||""})),
        stability_include_preop:s.stability_include_preop,
        stability_change_from:String(s.stability_change_from||""),
        stability_change_to:String(s.stability_change_to||""),
        stability_change_threshold:s.stability_change_threshold,
        astig_panels:s.astig_panels,
        double_angle_figs:s.double_angle_figs,
        semicircle_figs:s.semicircle_figs,
        astig_sheet:s.astig_sheet,
        astig_region:s.astig_region,
        astig_centroid_region:s.astig_centroid_region,
        astig_whwk_files:s.astig_whwk_files,
        astig_jcrs_standard:s.astig_jcrs_standard,
        cluster_by_patient:s.cluster_by_patient,
        astig_test_method:s.astig_test_method,
        astig_vector_cols: Object.fromEntries(Object.entries(s.astig_vector_cols).filter(([,v])=>v)),
        astig_target: Object.fromEntries(Object.entries(s.astig_target).filter(([,v])=>v)),
        do_regression:s.do_regression, regression_stage:s.regression_stage,
        regression_covariates_raw:s.regression_covariates_raw.slice(),
        roles:{...s.roles},
        dichotomous_preferred_level: {...s.dicho_map},
        output_dir:s.output_dir, output_prefix:s.output_prefix, export_filtered_csv:s.export_filtered_csv,
      };
    },
    applyCfg(c){
      if(!c) return;
      // atomic load: rebuild from pristine defaults, then merge the preset on
      // top - never onto whatever the previous study left behind
      this.s = defaultState();
      const s=this.s;
      this._subsetsFromCfg = true;   // the config's subsets (even none) are authoritative
      this.dichoGuessSrc = {}; this.dichoReplaced = {}; this.dichoUnseen = {};
      // null is a real value for these settings (empty caliper box = no caliper)
      const NULLABLE = new Set(["psm_caliper"]);
      const set=(k)=>{ if(c[k]===undefined) return; if(c[k]===null && !NULLABLE.has(k)) return; s[k]=c[k]; };
      ["guess_max","patient_id_raw","eye_id_raw","group_source_var",
       "age_var","fu_var","sex_var","do_matching","psm_covariates_raw","psm_method","psm_distance",
       "psm_ratio","psm_replace","psm_caliper","psm_std_caliper","psm_m_order","run_gee_stage",
       "run_main_gee","run_adjusted_gee","gee_adjust_covariates_raw","use_matching_weights_in_gee",
       "add_gee_columns","gee_source_for_tables","gee_small_sample","table_tests","subset_col_raw",
       "do_standardize_minus_cyl",
       "va_format","do_logmar","logmar_cols_raw",
       "make_meyestro","make_alpins","make_pub_table","output_dir","output_prefix","export_filtered_csv",
       "make_graphs","graph_set","graph_formats","graph_dpi",
       "figure_style","p_style",
       "stability_figs",
       "astig_panels","double_angle_figs","semicircle_figs","astig_sheet",
       "astig_region","astig_centroid_region","astig_whwk_files","astig_jcrs_standard",
       "cluster_by_patient","astig_test_method",
       "stability_include_preop","stability_change_threshold",
       "do_regression","regression_stage","regression_covariates_raw",
       "patient_level_params_raw","exclude_params_raw"].forEach(set);
      if(!VA_FORMATS.some(f=>f[0]===s.va_format)) s.va_format="decimal";
      // read as the engine reads them (trimmed, lower case; blank = default); an
      // unknown value is kept, so the run's check names it instead of hiding it.
      // A settings file without one of these keys (v1.0.0) gets the v1.1.0 default.
      for(const [k,d] of [["gee_small_sample","md"],["table_tests","auto"],
                          ["figure_style","journal"],["p_style","ama"],
                          ["psm_distance","linear.logit"],["astig_region","polygon"],
                          ["astig_centroid_region","bootstrap"],["astig_test_method","robust"]]){
        const v=String(s[k]??"").trim().toLowerCase(); s[k]= v || d; }
      // aliases the engine reads as the same setting
      if(s.astig_region==="depth") s.astig_region="polygon";
      if(s.p_style==="jcrs") s.p_style="ama";
      // the WHWK input files switch is read as the server reads it: blank = the
      // default (on), "true" / "false" (yes / no, on / off) text and 1 / 0 the
      // switch; any other value is kept, so the run's check names it
      { const wf=s.astig_whwk_files;
        if(typeof wf==="string"){
          const v=wf.trim().toLowerCase();
          if(!v) s.astig_whwk_files=true;
          else if(["true","false","1","0","yes","no","on","off"].includes(v)) s.astig_whwk_files=["true","1","yes","on"].includes(v);
        } else if(wf===0||wf===1) s.astig_whwk_files=wf===1; }
      // logMAR list (D16): a list the config chose is kept as it is - also an
      // empty one saved with the switch on by v1.0.0, which converted nothing;
      // a config without a list, or one that was following the roles
      // (logmar_cols_user false), follows the mapped VA roles again
      const lmc=c.logmar_cols_raw;
      s.logmar_cols_raw = Array.isArray(lmc) ? lmc.map(String) : [];
      this._logmarUser = Array.isArray(lmc) && (c.logmar_cols_user===true ||
        (c.logmar_cols_user===undefined && (lmc.length>0 || c.do_logmar!==false)));
      // figure width: blank (null / missing) = each figure's own size; a value is
      // kept as given so the server's check names one that is not a width
      s.figure_width_cm = (c.figure_width_cm===undefined||c.figure_width_cm===null) ? "" : String(c.figure_width_cm).trim();
      // a matching method the bundled R library cannot run (optimal, full,
      // genetic, ...) is replaced by nearest, said in an alert and under the
      // select; a blank method means nearest (as in the server and R), silently
      this.psmMethodNote="";
      const pm=String(s.psm_method??"").trim().toLowerCase();
      if(!pm) s.psm_method="nearest";
      else if(PSM_METHODS.some(m=>m[0]===pm)) s.psm_method=pm;
      else {
        this.psmMethodNote="This config asked for the matching method “"+String(s.psm_method)+
          "”, which this installation cannot run. Nearest-neighbor matching is used instead — check the settings below.";
        s.psm_method="nearest";
        alert(this.psmMethodNote);
      }
      // a config pairing a non-score distance with a caliper or a score order
      // would be refused by the server: adapt it, and say so
      const pfit=this.fitPsmToDistance();
      if(pfit){
        this.psmMethodNote=(this.psmMethodNote ? this.psmMethodNote+" " : "")+"This config: "+pfit;
        alert("This config: "+pfit);
      }
      // only the formats the engine writes; a hand-edited "jpg" / "PNG" is
      // normalised, anything else is dropped (and named, so nothing vanishes unseen)
      const GF=["tiff","png","jpeg","pdf","eps"];
      const rawFmt=Array.isArray(s.graph_formats) ? s.graph_formats : [];
      const fmts=rawFmt.map(f=>{ const x=String(f).trim().toLowerCase(); return x==="jpg" ? "jpeg" : (x==="tif" ? "tiff" : (x==="ps" ? "eps" : x)); });
      const badFmt=fmts.filter(f=>!GF.includes(f));
      s.graph_formats=[...new Set(fmts.filter(f=>GF.includes(f)))];
      if(!s.graph_formats.includes("tiff")) s.graph_formats.unshift("tiff");
      if(badFmt.length) alert("Unsupported figure format(s) in this config ignored: "+badFmt.join(", ")+". Available: TIFF, PNG, JPEG, PDF, EPS.");
      for(const sec of SECTIONS){
        s[sec]=Object.assign(emptyTyping(), c[sec]||{});
        for(const ty of TYPES) if(!Array.isArray(s[sec][ty])) s[sec][ty]=[];
        if(!Array.isArray(s[sec].order)) s[sec].order=[];
      }
      s.within_group = Object.assign(emptyTyping(), c.within_group||{});
      s.within_levels = {...(c.within_group_levels||{})};
      for(const ty of TYPES) if(!Array.isArray(s.within_group[ty])) s.within_group[ty]=[];
      // standardization triples: the config's sets are authoritative; a legacy
      // single sph/cyl/axis config becomes one set. An EMPTY list stays empty
      // only when the user emptied it (std_triples_user); otherwise it was
      // saved before the roles were mapped, and the sets are seeded from the
      // roles (below, and again whenever the roles change) as in a new session
      this._stdNoSeed = false;
      if(Array.isArray(c.std_triples) && c.std_triples.length){
        s.std_triples = c.std_triples.map(t=>withUid({sph:t.sph||"", cyl:t.cyl||"", axis:t.axis||""}));
        this._stdNoSeed = true;
      } else if(c.std_cyl_raw){
        s.std_triples = [withUid({sph:c.std_sph_raw||"", cyl:c.std_cyl_raw, axis:c.std_axis_raw||""})];
        this._stdNoSeed = true;
      } else {
        s.std_triples = [];
        this._stdNoSeed = Array.isArray(c.std_triples) && c.std_triples_user===true;
      }
      // stability visits: restored verbatim, never seeded (see suggestStabilityVisits);
      // a config predating the feature keeps the default (off)
      s.astig_vector_cols = {...(c.astig_vector_cols||{})};
      s.astig_target = {...(c.astig_target||{})};
      s.stability_visits = (Array.isArray(c.stability_visits) ? c.stability_visits : [])
        .map(v=>withUid({label:String(v.label??""), time:v.time||"", seq:v.seq||"",
                  sph:v.sph||"", cyl:v.cyl||""}));
      // the two change-pair <select>s are populated from the visit labels above,
      // so their <option>s do not exist yet on this tick and x-model would fall
      // back to "- none -". Re-assert once the options have rendered.
      const cf=String(c.stability_change_from??""), ct=String(c.stability_change_to??"");
      s.stability_change_from=""; s.stability_change_to="";
      this.$nextTick(()=>{ s.stability_change_from=cf; s.stability_change_to=ct; });
      // subset mode, read as R build_runs() reads it: an explicit subset_mode
      // wins (trimmed, lower-case; an unknown word means separate runs);
      // configs that carry only the old pooled toggle map to both/separate
      const sm=rTrim(c.subset_mode??"").toLowerCase();
      const sp=c.subset_pooled;
      s.subset_mode = sm ? (["separate","both","pooled"].includes(sm) ? sm : "separate")
                      : ((sp===true || ["TRUE","true","True","T"].includes(String(sp)) ||
                          (typeof sp==="number" && sp!==0)) ? "both" : "separate");
      if(c.groups){
        const gcols=c.group_colors||{}; const glabs=c.group_labels||{};
        s.groups = Object.entries(c.groups).map(([name,values],i)=>({name,
          values:Array.isArray(values)?values:[values],
          color: gcols[name] || DEFAULT_GROUP_COLORS[i % DEFAULT_GROUP_COLORS.length],
          fig_label: glabs[name] || "", _uid:++GROUP_UID}));
        if(s.groups.length<1) s.groups=[{name:"Group1",values:[],color:DEFAULT_GROUP_COLORS[0],fig_label:"",_uid:++GROUP_UID}];
      }
      // the saved reference NAME -> that row's identity; a name that matches no
      // group is kept only for the warning (the run waits for a choice). A
      // config with neither groups nor a reference keeps the default rows.
      if(c.groups || c.reference_group!==undefined){
        const want=(c.reference_group===undefined || c.reference_group===null) ? "" : String(c.reference_group);
        const g=want ? (s.groups.find(x=>x.name===want) || s.groups.find(x=>rTrim(x.name)===rTrim(want))) : null;
        s.reference_uid = g ? g._uid : null;
        s.reference_orphan = (g || !want) ? "" : want;
      }
      // subset_rows (GUI copy, keeps unticked rows) is used when it agrees with
      // the engine map; a hand-edited "subsets" that no longer matches wins
      const rowsAgree = rows => {
        const m={}; for(const g of rows) if(g.enabled!==false && g.name && Array.isArray(g.values) && g.values.length) m[g.name]=g.values.map(String);
        const e={}; for(const [k,v] of Object.entries(c.subsets||{})) e[k]=(Array.isArray(v)?v:[v]).map(String);
        return JSON.stringify(m)===JSON.stringify(e);
      };
      if(Array.isArray(c.subset_rows) && rowsAgree(c.subset_rows)){
        s.subsets = c.subset_rows.map(g=>withUid({name:String(g.name??""),
          values:Array.isArray(g.values)?g.values.slice():(g.values==null?[]:[g.values]), enabled:g.enabled!==false}));
      } else if(c.subsets){
        s.subsets = Object.entries(c.subsets).map(([name,values])=>withUid({name, values:Array.isArray(values)?values:[values], enabled:true}));
      } else s.subsets=[];
      if(c.filter_rules){
        s.filters = c.filter_rules.map(f=>{
          const row=withUid({var:f.var, op:f.op, value:f.value});
          this.coerceFilterValue(row);
          return row;
        });
        this.prefetchFilterDistincts();
      }
      if(c.roles){ s.roles = Object.assign(Object.fromEntries(Object.keys(ROLE_LABELS).map(k=>[k,""])), c.roles); }
      this.seedStdTriples();
      this.seedLogmarCols();
      if(c.psm_seed!==undefined && c.psm_seed!==null && c.psm_seed!=="" && isFinite(Number(c.psm_seed)))
        s.psm_seed=Math.trunc(Number(c.psm_seed));
      if(c.dichotomous_preferred_level){
        s.dicho_map = {};
        for(const [k,v] of Object.entries(c.dichotomous_preferred_level)) s.dicho_map[k]=String(v);
      }
      this.syncOrders();
      this.ensureDichoDefaults();
    },

    // ================= presets =================
    async refreshPresets(){ try{ const r=await fetch("/api/presets"); const j=await r.json(); this.presets=j.presets||[]; }catch(e){} },
    // An existing preset is replaced only after the user confirms (the server
    // answers 409 to overwrite:false). The name shown is the one the server
    // stored, which drops characters Windows forbids in file names.
    async savePreset(){
      // an empty box saves under the name it shows: the preset being worked on
      const name=(rTrim(this.presetName||"") || this.activePreset || rTrim(this.s.output_prefix||"") || "preset").trim();
      // the listed preset a stored name refers to (Windows file names ignore case)
      const listed=n=>(this.presets||[]).find(p=>String(p).toLowerCase()===String(n).toLowerCase()) || n;
      try{
        const post=overwrite=>fetch("/api/presets",{method:"POST",headers:{'Content-Type':'application/json'},
          body:JSON.stringify({name, cfg:this.buildCfg(), overwrite})});
        let r=await post(false);
        if(r.status===409){
          const d=await r.json().catch(()=>({}));
          const m=/'(.*)'/.exec(String(d.detail||""));
          const existing=listed(m ? m[1] : name);
          if(!confirm("A study preset named '"+existing+"' already exists. Replace its settings with the current ones?")) return;
          r=await post(true);
        }
        const j=await r.json().catch(()=>({detail:"HTTP "+r.status}));
        if(!r.ok){ alert(j.detail||"Save failed"); return; }
        this.presetName=""; await this.refreshPresets();
        const stored=listed(j.name||name);
        this.activePreset=stored;
        const renamed = (j.renamed!==undefined) ? !!j.renamed : (String(j.name||name)!==name);
        alert("Saved study preset '"+stored+"'"+(renamed ? " (the name '"+name+"' contains characters a file name cannot hold)" : "")+
              " — stored inside the app, webapp/presets folder.");
      }catch(e){ alert("Preset save failed: "+e); }
    },
    async loadPreset(name){
      if(!name) return;
      try{
        const r=await fetch("/api/presets/"+encodeURIComponent(name));
        const j=await r.json().catch(()=>({detail:"HTTP "+r.status}));
        if(!r.ok){ alert(j.detail||("Could not load preset '"+name+"'")); return; }
        await this.applyConfig(j.cfg); this.activePreset=j.name||name;
      }catch(e){ alert("Could not load preset '"+name+"': "+e); }
    },
    // Load a whole config (preset / JSON): FIRST switch to the sheet it was
    // made for, THEN bind the settings, so every per-column check (dichotomous
    // levels, distinct values) reads the new sheet, never the old one.
    async applyConfig(c){
      await this.applyCfgSheet(c);
      this.applyCfg(c);
      await this.onGroupSourceChange(); await this.onSubsetColChange();
      this.updateCounts(); this.detectVa();
    },
    // A config records the sheet it was made for (sheet_name). Switch to it
    // when the loaded workbook has it; remember it when no workbook is loaded
    // yet; say so when the workbook lacks it - never run on another sheet unseen.
    // A config without a sheet_name also forgets a sheet an earlier one left pending.
    async applyCfgSheet(c){
      this._pendingSheet=null;
      const want=c ? c.sheet_name : null;
      if(want===null || want===undefined || want==="") return;
      if(!this.file){ this._pendingSheet=want; return; }
      const name=this.resolveSheet(want, this.sheets);
      if(name===this.sheet) return;
      if(name){ this.sheet=name; this.pathInput=this.file; await this.loadFile(); }
      else alert("This config was made for the sheet “"+want+"”, which is not in the loaded workbook. It stays on the sheet “"+this.sheet+"” — check that it holds the same data.");
    },
    // a config names its sheet by name, or (as the engine allows) by 1-based position
    resolveSheet(want, sheets){
      sheets=sheets||[];
      if(typeof want==="number") return (Number.isInteger(want) && want>=1 && want<=sheets.length) ? sheets[want-1] : null;
      return sheets.includes(String(want)) ? String(want) : null;
    },
    async pickFolder(){
      try{
        const r=await fetch("/api/pick-folder",{method:"POST"});
        const j=await r.json();
        if(r.ok && j.path) this.s.output_dir=j.path;
        else if(!r.ok) alert(j.detail||"Could not open folder dialog. Type the path instead.");
      }catch(e){ alert("Folder dialog error: "+e); }
    },
    toggleFormat(fm){ this.toggle(this.s.graph_formats, fm); if(!this.s.graph_formats.includes("tiff")) this.s.graph_formats.push("tiff"); },
    async deletePreset(name){
      if(!name || !confirm("Delete preset '"+name+"'?")) return;
      try{
        const r=await fetch("/api/presets/"+encodeURIComponent(name),{method:"DELETE"});
        if(!r.ok){ const j=await r.json().catch(()=>({})); alert(j.detail||("Could not delete preset '"+name+"' (HTTP "+r.status+")")); }
        else if(this.activePreset===name) this.activePreset="";
      }catch(e){ alert("Preset delete failed: "+e); }
      await this.refreshPresets();
    },
    downloadCfg(){
      const blob=new Blob([JSON.stringify(this.buildCfg(),null,2)],{type:"application/json"});
      const a=document.createElement("a"); a.href=URL.createObjectURL(blob);
      a.download=(this.s.output_prefix||"config")+".json"; a.click();
    },
    async uploadCfg(ev){
      const f=ev.target.files[0]; if(!f) return;
      const txt=await f.text();
      let c;
      try{ c=JSON.parse(txt); }
      catch(e){ alert("Invalid config JSON: "+e); ev.target.value=""; return; }
      // the settings no longer are those of the preset worked on so far: an
      // empty name box must not save them over that preset
      this.activePreset="";
      try{ await this.applyConfig(c); }
      catch(e){ alert("Could not apply this config: "+e); }
      ev.target.value="";
    },

    // ================= run =================
    async run(){
      if(!this.file){ alert("Load a data file first."); this.tab="data"; return; }
      if(this.groupNames().length<2 || this.s.groups.filter(g=>g.values.length).length<2){
        alert("Define at least 2 groups, each with values."); this.tab="data"; return; }
      const dupGroups=this.dupGroupNames();
      if(dupGroups.length){
        alert("Group names must be unique ("+dupGroups.join(", ")+" is used twice) — rename one (Data & Groups tab).");
        this.tab="data"; return; }
      const subsetProblems=this.subsetNameProblems();
      if(subsetProblems.length){
        alert("Subset names: "+subsetProblems.join("; ")+" (Filters & Subsets tab).");
        this.tab="filters"; return; }
      const dupLabels=this.stabilityLabelDupes();
      if(dupLabels.length){
        alert("Stability visits share a label ("+dupLabels.join(", ")+"). Give each visit a unique label (Special outputs tab).");
        this.tab="special"; return; }
      if(this.subsetColMissing()){
        alert("The subset column '"+this.s.subset_col_raw+"' is not in this file. Re-map it or clear it (Filters & Subsets tab).");
        this.tab="filters"; return; }
      if(this.figureWidthProblem()){
        alert("Figure width: "+this.figureWidthProblem()+" (Special outputs tab).");
        this.tab="special"; return; }
      // the reference decides matching direction and every "vs reference"
      // effect: never let it fall back to another group unseen
      const refProblem=this.referenceProblem();
      if(refProblem){ alert(refProblem); this.tab="data"; return; }
      const badRules=this.s.filters.map((f,i)=>[i,this.filterValueProblem(f)]).filter(x=>x[1]);
      if(badRules.length){
        alert("Filter rule "+badRules.map(([i,p])=>(i+1)+" ("+this.s.filters[i].var+"): "+p).join("; ")+".");
        this.tab="filters"; return; }
      if(this.running) return;
      // settings naming columns this file does not have are dropped by the
      // engine: say which before the run, not after it
      const unresolved=this.unresolvedMappings().filter(c=>c!==this.s.subset_col_raw);
      if(unresolved.length && !confirm(unresolved.length+" mapped column(s) are not in this file and will be ignored:\n\n"+
           unresolved.slice(0,25).join("\n")+(unresolved.length>25 ? "\n… and "+(unresolved.length-25)+" more" : "")+
           "\n\nThe rows, tables or figures that need them will be blank. Run anyway?")) return;
      this.running=true; this.progress=0; this.progressMsg="Starting…"; this.log=[]; this.runError=null; this.results=null; this.tab="run";
      this.savedFiles={}; this.exportMsg=null; this._resultSeen=false; this._resultFiles=0; this._exitCode=null;
      this.cancelling=false; this.serverBusy=false; this._busy409=false; clearTimeout(this._busyT);
      this._figSeq++; this.figures={busy:false, dirs:[], error:null};
      // the sheet preview showed the previous run's workbook (and a reply
      // still on its way belongs to it too)
      this._sheetSeq++; this.resultSheet={file:null,sheet:null,data:null,error:null};
      // the previous run's matched files are about to be replaced
      this.hist.mfile=""; if(this.hist.stage==="matched") this.hist.stage="filtered";
      const ctl=(typeof AbortController!=="undefined") ? new AbortController() : null;
      this._runCtl=ctl;
      try{
        const resp=await fetch("/api/run",{method:"POST",headers:{'Content-Type':'application/json'},
          body:JSON.stringify({cfg:this.buildCfg(), preview_only:this.previewOnly}),
          signal: ctl ? ctl.signal : undefined});
        if(!resp.ok){
          const j=await resp.json().catch(()=>({detail:resp.statusText}));
          this.runError=j.detail||"Run failed";
          this.running=false; this._runCtl=null;
          // offer to stop the run that is still going, and notice when it ends
          if(resp.status===409){ this.serverBusy=true; this._busy409=true; this.watchBusy(); }
          return;
        }
        const reader=resp.body.getReader(); const dec=new TextDecoder(); let buf="";
        while(true){
          const {value,done}=await reader.read(); if(done) break;
          buf+=dec.decode(value,{stream:true});
          let idx;
          while((idx=buf.indexOf("\n\n"))>=0){
            const chunk=buf.slice(0,idx); buf=buf.slice(idx+2);
            const line=chunk.split("\n").find(l=>l.startsWith("data:"));
            if(!line) continue;
            this.handleEvent(JSON.parse(line.slice(5).trim()));
          }
        }
      }catch(e){
        // an aborted stream after Cancel is the expected end, not a crash
        if(!this.runError) this.runError = (e && e.name==="AbortError") || this.cancelling
          ? "Analysis canceled. Nothing from this run is shown." : String(e);
      }
      this.running=false; this._runCtl=null;
      clearTimeout(this._cancelT);
      if(this.cancelling && !this.runError) this.runError="Analysis canceled. Nothing from this run is shown.";
      this.cancelling=false;
      // success requires a zero exit code, an actual result payload AND at
      // least one workbook - an R crash that skips the protocol's ERROR line, or
      // a run whose every subset was skipped, must not look like a success
      if(!this.runError && this._exitCode!==null && this._exitCode!==0)
        this.runError="R exited with code "+this._exitCode+" — see the run log above.";
      if(!this.runError && !this._resultSeen)
        this.runError="The analysis ended without producing results — see the run log above.";
      if(!this.runError && this._resultFiles===0)
        this.runError="The run finished without writing any workbook — see the run log above for the runs that were skipped.";
      if(!this.runError){ this.progress=1; await this.loadResults(); }
    },
    // Stop the running analysis: the server terminates R (and its children);
    // the stream then ends with a "cancelled" error. If it does not end within
    // a few seconds the stream is dropped, which also makes the server stop R.
    async cancelRun(){
      if(this.cancelling) return;
      if(!this.running && !this.serverBusy) return;
      this.cancelling=true; this.progressMsg="Stopping…";
      // {running, terminated}: was anything running, and was it stopped?
      let j=null;
      try{ const r=await fetch("/api/cancel",{method:"POST"}); if(r.ok) j=await r.json().catch(()=>null); }catch(e){}
      if(!this.running){            // a run started before this page was loaded
        this.cancelling=false;
        if(!j){
          this.progressMsg="Could not reach the server to stop the earlier analysis — try again.";
        } else if(j.running && j.terminated===false){
          this.progressMsg="The server could not stop the earlier analysis — wait for it to finish.";
        } else {
          this.serverBusy=false; this._busy409=false; this.runError=null; clearTimeout(this._busyT);
          this.progressMsg = j.running ? "The earlier analysis was stopped — you can run again."
                                       : "No analysis was running any more — you can run again.";
        }
        this.watchBusy();
        return;
      }
      clearTimeout(this._cancelT);
      this._cancelT=setTimeout(()=>{ if(this.running && this._runCtl) this._runCtl.abort(); }, 5000);
    },
    handleEvent(ev){
      if(ev.type==="progress"){ if(ev.fraction!=null) this.progress=ev.fraction; this.progressMsg=ev.message; this.log.push("• "+ev.message); }
      else if(ev.type==="log"){ this.log.push(ev.message); }
      else if(ev.type==="debug"){ this.log.push("  "+ev.message); }
      // the engine flattens multi-line R errors into one protocol line joined
      // by " | "; show them on separate lines again
      else if(ev.type==="error"){ const m=String(ev.message||"").split(" | ").join("\n"); this.runError=m; this.log.push("ERROR: "+m); }
      else if(ev.type==="result"){
        const files=(ev.data && Array.isArray(ev.data.files)) ? ev.data.files : [];
        this._resultSeen=true; this._resultFiles=files.length;
        this.log.push("Saved "+files.length+" workbook(s).");
      }
      else if(ev.type==="done"){ this._exitCode=ev.returncode; }
      this.$nextTick(()=>{ const el=document.getElementById("logbox"); if(el) el.scrollTop=el.scrollHeight; });
    },
    // ---- pre-run summary (Run & Results tab): the settings that decide how
    // the numbers are computed and printed, in words. Mirrors the engine's
    // reading of them (blank / unknown = the default the server then names).
    runSummary(){
      const s=this.s, out=[];
      const pid=!!s.patient_id_raw && this.colResolves(s.patient_id_raw);
      const dist=rTrim(s.psm_distance??"").toLowerCase() || "linear.logit";
      const scale = NON_PS_DISTANCES.includes(dist) ? dist+" distance (not a propensity score)"
        : dist.startsWith("linear.") ? (dist==="linear.logit" ? "logit of the propensity score" : "linear predictor ("+dist.slice(7)+" link) of the propensity score")
        : "propensity score on the probability scale";
      const cal = s.psm_caliper===null || s.psm_caliper===undefined || s.psm_caliper==="";
      out.push(["Matching", !s.do_matching ? "off"
        : !this.psmUses('distance') ? s.psm_method+" (no distance or caliper)"
        : s.psm_method+" on the "+scale+(this.psmUses('caliper') && !cal
            ? "; caliper "+s.psm_caliper+(s.psm_std_caliper ? " SD of the "+(NON_PS_DISTANCES.includes(dist) ? "distance" : scale) : " (raw units)")
            : (this.psmUses('caliper') ? "; no caliper" : ""))]);
      out.push(["GEE inference", s.gee_small_sample==="none"
        ? "uncorrected robust SE, normal reference (v1.0.0)"
        : "Mancl-DeRouen robust SE, t reference (F for joint tests)"]);
      out.push(["Table tests", s.table_tests==="classical" ? "classical (v1.0.0)"
        : !s.cluster_by_patient ? "classical - patient clustering is switched off (Special outputs)"
        : !pid ? "classical - no patient ID column is mapped"
        : "clustered by patient (GEE) where a patient has two analyzed eyes; classical otherwise"]);
      out.push(["P values", s.p_style==="classic" ? "classic: 0.266, <0.001 (v1.0.0)" : "journal: .27, .003, <.001, >.99"]);
      out.push(["Visual acuity", !s.do_logmar ? "analyzed as recorded ("+this.vaFormatLabel(s.va_format)+")"
        : s.va_format==="logmar" ? "recorded in logMAR, analyzed as recorded"
        : "logMAR for "+s.logmar_cols_raw.length+" column(s), shown as Snellen with the logMAR mean ± SD"]);
      if(s.make_graphs){
        const w=String(s.figure_width_cm??"").trim();
        out.push(["Figures", (s.figure_style==="annotated" ? "annotated (v1.0.0 look)" : "journal style, legend text in <figure>_legend.txt")+
          "; "+(w ? w+" cm wide" : "each figure's own size")+"; "+s.graph_formats.map(f=>f.toUpperCase()).join(", ")+
          " at "+s.graph_dpi+" dpi"]);
      } else out.push(["Figures", "off"]);
      const reg={polygon:"95% depth-based convex polygon (as in the WHWK functions)", normal:"95% normal ellipse",
                 both:"depth-based convex polygon and normal ellipse"}[s.astig_region] || s.astig_region;
      const cr={bootstrap:"bootstrap convex polygon (as in the WHWK functions)", hotelling:"Hotelling T² ellipse"}[s.astig_centroid_region] || s.astig_centroid_region;
      const unit=(s.cluster_by_patient && pid) ? "patients" : "eyes";
      const tm={robust:"robust (centroid vs 0 as in the WHWK functions; between cohorts trimmed; bootstrap of "+unit+")",
                gee:"clustered GEE Wald", classic:"classic (eyes independent)"}[s.astig_test_method] || s.astig_test_method;
      out.push(["Astigmatism", "region of the eyes: "+reg+"; centroid CI: "+cr+"; tests: "+tm+
        "; JCRS 2025 sheet "+(s.astig_sheet ? "on" : "off")+
        "; WHWK input files "+((s.make_graphs && s.double_angle_figs && s.astig_whwk_files) ? "on" : "off")]);
      return out;
    },
    // the run's notes (server checks before R starts) and the engine's
    // WARNING lines, shown above the log so they are not missed in it
    runNotes(){
      return (this.log||[]).map(String).filter(l=>/^(Note: |WARNING)/.test(l)).map(l=>l.replace(/^Note: /,""));
    },
    async loadResults(){
      try{
        const r=await fetch("/api/results");
        const j=await r.json().catch(()=>({detail:"HTTP "+r.status}));
        if(r.ok) this.results=j;
        else this.runError="Could not load results: "+(j.detail||("HTTP "+r.status));
      }catch(e){ this.runError="Could not load results: "+e; }
      this.syncHistMatched();
      if(this.hist.col) this.showHist();
      await this.loadFigures();
    },
    // low-resolution PNG previews of every composite figure (GET /api/figures);
    // written by the engine whatever graph formats were chosen
    async loadFigures(){
      const seq=++this._figSeq;
      this.figures={busy:true, dirs:[], error:null};
      try{
        const r=await fetch("/api/figures");
        const j=await r.json().catch(()=>({detail:r.statusText}));
        if(seq!==this._figSeq) return;
        if(r.ok) this.figures={busy:false, dirs:Array.isArray(j.dirs)?j.dirs:[], error:null};
        else this.figures={busy:false, dirs:[], error:j.detail||"Could not list the figures"};
      }catch(e){ if(seq===this._figSeq) this.figures={busy:false, dirs:[], error:String(e)}; }
    },
    // previews for one graph folder of /api/results (matched by path, then name)
    figurePreviews(gd){
      const d=(this.figures.dirs||[]).find(x=>x.path===gd.path) || (this.figures.dirs||[]).find(x=>x.name===gd.name);
      return (d && Array.isArray(d.previews)) ? d.previews : [];
    },
    async viewSheet(file,sheet){
      const seq=++this._sheetSeq;
      this.resultSheet={file,sheet,data:null,error:null};
      try{
        const r=await fetch("/api/results/sheet?file="+encodeURIComponent(file)+"&sheet="+encodeURIComponent(sheet));
        const j=await r.json().catch(()=>({detail:r.statusText}));
        if(seq!==this._sheetSeq) return;   // the user already asked for another sheet
        if(r.ok) this.resultSheet.data=j;
        else this.resultSheet.error=j.detail||"Could not read this sheet";
      }catch(e){ if(seq===this._sheetSeq) this.resultSheet.error=String(e); }
    },
    downloadUrl(file){ return "/api/download?file="+encodeURIComponent(file); },

    // ---- selective export of preview-only results ----
    async exportFiles(paths, all=false){
      if(!paths.length) return;
      if(!this.s.output_dir){ alert("Set the output folder first (sidebar)."); return; }
      this.exporting=true; this.exportMsg=null;
      try{
        const r=await fetch("/api/export",{method:"POST",headers:{'Content-Type':'application/json'},
          body:JSON.stringify({files:paths, dest:this.s.output_dir, include_sidecars:all})});
        const j=await r.json().catch(()=>({detail:r.statusText}));
        if(r.ok){
          // mark only what was actually copied (a workbook open in Excel is
          // reported in j.failed and stays unsaved)
          const base=p=>String(p).split(/[\\/]/).pop().toLowerCase();
          const done=new Set((j.saved||[]).map(base));
          let nWb=0;
          for(const p of paths) if(done.has(base(p))){ this.savedFiles[p]=true; nWb++; }
          // j.saved also lists the extra files (figures folder, PNGs, filtered
          // CSV) copied with "save all": count the workbooks on their own
          const nOther=Math.max(0, (j.saved||[]).length-nWb);
          const failed=Array.isArray(j.failed)?j.failed:[];
          this.exportMsg = nWb+" workbook(s)"+(nOther ? " and "+nOther+" other file(s) / folder(s)" : "")+
            " saved to "+j.dest + (failed.length ? " — NOT saved: "+failed.join("; ") : "");
          this.exportOk = !failed.length;
        } else { this.exportMsg = "Export failed: "+(j.detail||""); this.exportOk=false; }
      }catch(e){ this.exportMsg="Export failed: "+e; this.exportOk=false; }
      this.exporting=false;
    },
    async exportAll(){
      const all=[...(this.results.files||[]), ...(this.results.matched_files||[])].map(f=>f.path);
      return this.exportFiles(all, true);
    },

    // ---- standalone filtered-data export (Data & Groups tab) ----
    async exportFiltered(){
      if(!this.file){ alert("Load a data file first."); return; }
      this.filteredExport={busy:true, path:null, rows:null, error:null};
      try{
        const r=await fetch("/api/export-filtered",{method:"POST",headers:{'Content-Type':'application/json'},
          body:JSON.stringify({cfg:this.buildCfg(), dest:this.s.output_dir})});
        const j=await r.json();
        // grouped false: no group is defined yet, so every row passing the filters and subsets was written
        if(r.ok) this.filteredExport={busy:false, path:j.path, rows:j.rows, error:null, grouped:j.grouped!==false};
        else this.filteredExport={busy:false, path:null, rows:null, error:j.detail||"export failed"};
      }catch(e){ this.filteredExport={busy:false, path:null, rows:null, error:String(e)}; }
    },
  };
}
// x-data="builder()" / x-data="combo(...)" resolve through these registrations
document.addEventListener("alpine:init", () => {
  Alpine.data("builder", builder);
  Alpine.data("combo", combo);
});
