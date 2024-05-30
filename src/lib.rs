//! # CFG Aliases
//!
//! CFG Aliases is a tiny utility to help save you a lot of effort with long winded `#[cfg()]` checks. This crate provides a single [`cfg_aliases!`] macro that doesn't have any dependencies and specifically avoids pulling in `syn` or `quote` so that the impact on your comile times should be negligible.
//!
//! You use the the [`cfg_aliases!`] macro in your `build.rs` script to define aliases such as `x11` that could then be used in the `cfg` attribute or macro for conditional compilation: `#[cfg(x11)]`.
//!
//! ## Example
//!
//! **Cargo.toml:**
//!
//! ```toml
//! [build-dependencies]
//! cfg_aliases = "0.1.0"
//! ```
//!
//! **build.rs:**
//!
//! ```rust
//! use cfg_aliases::cfg_aliases;
//!
//! fn main() {
//!     // Setup cfg aliases
//!     cfg_aliases! {
//!         // Platforms
//!         wasm: { target_arch = "wasm32" },
//!         android: { target_os = "android" },
//!         macos: { target_os = "macos" },
//!         linux: { target_os = "linux" },
//!         // Backends
//!         surfman: { all(unix, feature = "surfman", not(wasm)) },
//!         glutin: { all(feature = "glutin", not(wasm)) },
//!         wgl: { all(windows, feature = "wgl", not(wasm)) },
//!         dummy: { not(any(wasm, glutin, wgl, surfman)) },
//!     }
//! }
//! ```
//!
//! Now that we have our aliases setup we can use them just like you would expect:
//!
//! ```rust
//! #[cfg(wasm)]
//! println!("This is running in WASM");
//!
//! #[cfg(surfman)]
//! {
//!     // Do stuff related to surfman
//! }
//!
//! #[cfg(dummy)]
//! println!("We're in dummy mode, specify another feature if you want a smarter app!");
//! ```
//!
//! This greatly improves what would otherwise look like this without the aliases:
//!
//! ```rust
//! #[cfg(target_arch = "wasm32")]
//! println!("We're running in WASM");
//!
//! #[cfg(all(unix, feature = "surfman", not(target_arch = "22")))]
//! {
//!     // Do stuff related to surfman
//! }
//!
//! #[cfg(not(any(
//!     target_arch = "wasm32",
//!     all(unix, feature = "surfman", not(target_arch = "wasm32")),
//!     all(windows, feature = "wgl", not(target_arch = "wasm32")),
//!     all(feature = "glutin", not(target_arch = "wasm32")),
//! )))]
//! println!("We're in dummy mode, specify another feature if you want a smarter app!");
//! ```
//!
//! You can also use the `cfg!` macro or combine your aliases with other checks using `all()`, `not()`, and `any()`. Your aliases are genuine `cfg` flags now!
//!
//! ```rust
//! if cfg!(glutin) {
//!     // use glutin
//! } else {
//!     // Do something else
//! }
//!
//! #[cfg(all(glutin, surfman))]
//! compile_error!("You cannot specify both `glutin` and `surfman` features");
//! ```
//!
//! ## Syntax and Error Messages
//!
//! The aliase names are restricted to the same rules as rust identifiers which, for one, means that they cannot have dashes ( `-` ) in them. Additionally, if you get certain syntax elements wrong, such as the alias name, the macro will error saying that the recursion limit was reached instead of giving a clear indication of what actually went wrong. This is due to a nuance with the macro parser and it might be fixed in a later release of this crate. It is also possible that aliases with dashes in the name might be supported in a later release. Open an issue if that is something that you would like implemented.
//!
//! Finally, you can also induce an infinite recursion by having rules that both reference each-other, but this isn't a real limitation because that doesn't make logical sense anyway:
//!
//! ```rust,ignore
//! // This causes an error!
//! cfg_aliases! {
//!     test1: { not(test2) },
//!     test2: { not(test1) },
//! }
//! ```
//!
//! ## Attribution and Thanks
//!
//! - Thanks to my God and Father who led me through figuring this out and to whome I owe everything.
//! - Thanks to @Yandros on the Rust forum for [showing me][sm] some crazy macro hacks!
//! - Thanks to @sfackler for [pointing out][po] the way to make cargo add the cfg flags.
//! - Thanks to the authors of the [`tectonic_cfg_support::target_cfg`] macro from which most of the cfg attribute parsing logic is taken from. Also thanks to @ratmice for [bringing it up][bip] on the Rust forum.
//!
//! [`tectonic_cfg_support::target_cfg`]: https://docs.rs/tectonic_cfg_support/0.0.1/src/tectonic_cfg_support/lib.rs.html#166-298
//! [po]: https://users.rust-lang.org/t/any-such-thing-as-cfg-aliases/40100/2
//! [bip]: https://users.rust-lang.org/t/any-such-thing-as-cfg-aliases/40100/13
//! [sm]: https://users.rust-lang.org/t/any-such-thing-as-cfg-aliases/40100/3

#![allow(clippy::needless_doctest_main)]

// In the `cfg_aliases!` macro below, all of the rules that start with @parser were derived from
// the `target_cfg!` macro here:
//
// https://docs.rs/tectonic_cfg_support/0.0.1/src/tectonic_cfg_support/lib.rs.html#166-298.
//
// The `target_cfg!` macro is excellently commented while the one below is not very well commented
// yet, so if you need some help understanding it you might benefit by reading that implementation.
// Also check out this forum topic for more history on the macro development:
//
// https://users.rust-lang.org/t/any-such-thing-as-cfg-aliases/40100?u=zicklag

/// Create `cfg` aliases
///
/// **build.rs:**
///
/// ```rust
/// # use cfg_aliases::cfg_aliases;
/// // Setup cfg aliases
/// cfg_aliases! {
///     // Platforms
///     wasm: { target_arch = "wasm32" },
///     android: { target_os = "android" },
///     macos: { target_os = "macos" },
///     linux: { target_os = "linux" },
///     // Backends
///     surfman: { all(unix, feature = "surfman", not(wasm)) },
///     glutin: { all(feature = "glutin", not(wasm)) },
///     wgl: { all(windows, feature = "wgl", not(wasm)) },
///     dummy: { not(any(wasm, glutin, wgl, surfman)) },
/// }
/// ```
///
/// After you put this in your build script you can then check for those conditions like so:
///
/// ```rust
/// #[cfg(surfman)]
/// {
///     // Do stuff related to surfman
/// }
///
/// #[cfg(dummy)]
/// println!("We're in dummy mode, specify another feature if you want a smarter app!");
/// ```
///
/// This greatly improves what would otherwise look like this without the aliases:
///
/// ```rust
/// #[cfg(all(unix, feature = "surfman", not(target_arch = "wasm32")))]
/// {
///     // Do stuff related to surfman
/// }
///
/// #[cfg(not(any(
///     target_arch = "wasm32",
///     all(unix, feature = "surfman", not(target_arch = "wasm32")),
///     all(windows, feature = "wgl", not(target_arch = "wasm32")),
///     all(feature = "glutin", not(target_arch = "wasm32")),
/// )))]
/// println!("We're in dummy mode, specify another feature if you want a smarter app!");
/// ```
#[macro_export]
macro_rules! cfg_aliases {
    // Emitting `any(clause1,clause2,...)`: convert to `$crate::cfg_aliases!(clause1) && $crate::cfg_aliases!(clause2) && ...`
    (
        @parser_emit,
        all
        $({$($grouped:tt)+})+
    ) => {{
        let mut possible = $crate::True;
        let mut cfgs = vec![];
        let mut parsed = vec![];
        $(
            let $crate::Ret { cfg, parsed: pp, possible: p } = $crate::cfg_aliases!(@parser, $($grouped)+);
            cfgs.extend(cfg);
            parsed.push(pp);
            possible &= p;
        )*
        let parsed = $crate::Cfg::All(parsed);
        if cfgs.is_empty() {
            $crate::Ret { cfg: None, parsed, possible }
        } else {
            $crate::Ret { cfg: Some($crate::Cfg::All(cfgs)), parsed, possible }
        }
    }};

    // Likewise for `all(clause1,clause2,...)`.
    (
        @parser_emit,
        any
        $({$($grouped:tt)+})+
    ) => {{
        let mut possible = $crate::False;
        let mut cfgs = vec![];
        let mut parsed = vec![];
        $(
            let $crate::Ret { cfg, parsed: pp, possible: p } = $crate::cfg_aliases!(@parser, $($grouped)+);
            cfgs.extend(cfg);
            parsed.push(pp);
            possible |= p;
        )*
        let parsed = $crate::Cfg::Any(parsed);
        if cfgs.is_empty() {
            $crate::Ret { cfg: None, parsed, possible }
        } else {
            $crate::Ret { cfg: Some($crate::Cfg::Any(cfgs)), parsed, possible }
        }
    }};

    // "@clause" rules are used to parse the comma-separated lists. They munch
    // their inputs token-by-token and finally invoke an "@emit" rule when the
    // list is all grouped. The general pattern for recording the parser state
    // is:
    //
    // ```
    // $crate::cfg_aliases!(
    //    @clause $operation
    //    [{grouped-clause-1} {grouped-clause-2...}]
    //    [not-yet-parsed-tokens...]
    //    current-clause-tokens...
    // )
    // ```

    // This rule must come first in this section. It fires when the next token
    // to parse is a comma. When this happens, we take the tokens in the
    // current clause and add them to the list of grouped clauses, adding
    // delimeters so that the grouping can be easily extracted again in the
    // emission stage.
    (
        @parser_clause,
        $op:ident
        [$({$($grouped:tt)+})*]
        [, $($rest:tt)*]
        $($current:tt)+
    ) => {
        $crate::cfg_aliases!(@parser_clause, $op [
            $(
                {$($grouped)+}
            )*
            {$($current)+}
        ] [
            $($rest)*
        ])
    };

    // This rule comes next. It fires when the next un-parsed token is *not* a
    // comma. In this case, we add that token to the list of tokens in the
    // current clause, then move on to the next one.
    (
        @parser_clause,
        $op:ident
        [$({$($grouped:tt)+})*]
        [$tok:tt $($rest:tt)*]
        $($current:tt)*
    ) => {
        $crate::cfg_aliases!(@parser_clause, $op [
            $(
                {$($grouped)+}
            )*
        ] [
            $($rest)*
        ] $($current)* $tok)
    };

    // This rule fires when there are no more tokens to parse in this list. We
    // finish off the "current" token group, then delegate to the emission
    // rule.
    (
        @parser_clause,
        $op:ident
        [$({$($grouped:tt)+})*]
        []
        $($current:tt)+
    ) => {
        $crate::cfg_aliases!(@parser_emit, $op
            $(
                {$($grouped)+}
            )*
            {$($current)+}
        )
    };


    // `all(clause1, clause2...)` : we must parse this comma-separated list and
    // partner with `@emit all` to output a bunch of && terms.
    (
        @parser,
        all($($tokens:tt)+)
    ) => {
        $crate::cfg_aliases!(@parser_clause, all [] [$($tokens)+]);
    };

    // Likewise for `any(clause1, clause2...)`
    (
        @parser,
        any($($tokens:tt)+)
    ) => {
        $crate::cfg_aliases!(@parser_clause, any [] [$($tokens)+])
    };

    // `not(clause)`: compute the inner clause, then just negate it.
    (
        @parser,
        not($($tokens:tt)+)
    ) => {{
        let $crate::Ret { cfg, parsed, possible } = $crate::cfg_aliases!(@parser, $($tokens)+);
        $crate::Ret { cfg: cfg.map($crate::Cfg::not), parsed: $crate::Cfg::not(parsed), possible: !possible }
    }};

    // `feature = value`: test for a feature.
    (@parser, feature = $value:expr) => {{
        let possible;
        #[cfg(feature = $value)] { possible = $crate::True; }
        #[cfg(not(feature = $value))] { possible = $crate::False; }
        $crate::Ret {
            cfg: None,
            parsed: $crate::Cfg::Feature(stringify!($value)),
            possible,
        }
    }};
    // `param = value`: test for equality.
    (@parser, $key:ident = $value:expr) => {{
            let cfg = $crate::Cfg::Contains(stringify!($key), stringify!($value));
            $crate::Ret { cfg: Some(cfg.clone()), parsed: cfg, possible: $crate::Maybe }
    }};
    // Parse a lone identifier that might be an alias
    (@parser, $e:ident) => {
        __cfg_aliases_matcher__!($e);
    };

    // Entrypoint that defines the matcher
    (
        @with_dollar[$dol:tt]
        $( $alias:ident : { $($config:tt)* } ),* $(,)?
    ) => {
        // Create a macro that expands other aliases and outputs any non
        // alias by checking whether that CFG value is set
        macro_rules! __cfg_aliases_matcher__ {
            // Parse config expression for the alias
            $(
                ( $alias ) => {{
                    let mut ret = $crate::cfg_aliases!(@parser, $($config)*);
                    ret.parsed = $crate::Cfg::Set(stringify!($alias));
                    ret
                }};
            )*
            // Anything that doesn't match evaluate the item
            ( $dol e:ident ) => {{
                let cfg = $crate::Cfg::Set(stringify!($dol e));
                $crate::Ret { cfg: Some(cfg.clone()), parsed: cfg, possible: $crate::Maybe }
            }};
        }

        let mut unconditional_cfgs = vec![];
        let mut platform_fixups: ::std::collections::BTreeMap<String, (i32, Vec<&'static str>)> = ::std::collections::BTreeMap::new();

        let mut i = 0;
        $({
            fn $alias() -> $crate::Ret {
                $crate::cfg_aliases!(@parser, $($config)*)
            }
            let ret = $alias();
            match ret {
                $crate::Ret { possible: $crate::False, .. } => {}
                $crate::Ret { possible: $crate::True, parsed, .. } => {
                    unconditional_cfgs.push((stringify!($alias), parsed));
                }
                $crate::Ret { possible: $crate::Maybe, cfg: Some(cfg), parsed: _ } => {
                    let alias = stringify!($alias);
                    let cfg = cfg.normalise();
                    let key = format!("{cfg}");
                    platform_fixups.entry(key).or_insert_with(|| {
                        i += 1;
                        (i, vec![])
                    }).1.push(alias);
                }
                $crate::Ret { possible: $crate::Maybe, cfg: None, .. } => {}
            }
        })*

        if !unconditional_cfgs.is_empty() {
            let cfgs = unconditional_cfgs.iter().map(|c| c.0).collect::<Vec<_>>();
            println!("cfgs = {:?}", cfgs);
            println!();
        }

        let mut sortable = platform_fixups.into_iter().collect::<Vec<_>>();
        sortable.sort_by_key(|(_, (ord, _))| *ord);

        for (cfg_key, (order, alias)) in sortable {
            println!(
                "[platform_fixup.'cfg({})']\ncfgs = {:?}\n",
                cfg_key,
                alias
            )
        }
    };

    // Catch all that starts the macro
    ($($tokens:tt)*) => {
        $crate::cfg_aliases!(@with_dollar[$] $($tokens)*)
    }
}

pub struct Ret {
    pub cfg: Option<Cfg>,
    pub parsed: Cfg,
    pub possible: Tristate,
}

type Name = &'static str;
#[derive(Clone)]
pub enum Cfg {
    Set(Name),
    Feature(Name),
    Contains(Name, Name),
    Not(Box<Cfg>),
    Any(Vec<Cfg>),
    All(Vec<Cfg>),
}
impl Cfg {
    pub fn not(cfg: Self) -> Self {
        Cfg::Not(Box::new(cfg))
    }
    pub fn normalise(self) -> Self {
        match self {
            Self::All(cfgs) if cfgs.len() == 1 => cfgs.into_iter().nth(0).unwrap().normalise(),
            Self::Any(cfgs) if cfgs.len() == 1 => cfgs.into_iter().nth(0).unwrap().normalise(),
            Self::All(cfgs) => Self::All(cfgs.into_iter().map(Cfg::normalise).collect()),
            Self::Any(cfgs) => Self::Any(cfgs.into_iter().map(Cfg::normalise).collect()),
            Self::Not(cfg) => Self::Not(Box::new(cfg.normalise())),
            _ => self,
        }
    }
}
impl fmt::Display for Cfg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Set(cfg) => f.write_str(cfg),
            Self::Feature(feat) => write!(f, "feature = {feat}"),
            Self::Contains(k, v) => write!(f, "{k} = {v}"),
            Self::Not(cfg) => {
                write!(f, "not({})", cfg)
            }
            Self::All(cfgs) => {
                let mut any = false;
                write!(f, "all(")?;
                for x in cfgs {
                    if any {
                        f.write_str(", ")?;
                    }
                    x.fmt(f)?;
                    any = true;
                }
                write!(f, ")")
            }
            Self::Any(cfgs) => {
                let mut any = false;
                write!(f, "any(")?;
                for x in cfgs {
                    if any {
                        f.write_str(", ")?;
                    }
                    x.fmt(f)?;
                    any = true;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Tristate {
    True,
    False,
    Maybe,
}

use std::fmt;

pub use Tristate::*;

impl std::ops::BitOr for Tristate {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (True, _) | (_, True) => True,
            (Maybe, _) | (_, Maybe) => Maybe,
            _ => False,
        }
    }
}

impl std::ops::BitAnd for Tristate {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (True, True) => True,
            (_, False) | (False, _) => False,
            (Maybe, _) | (_, Maybe) => Maybe,
        }
    }
}

impl std::ops::BitOrAssign for Tristate {
    fn bitor_assign(&mut self, rhs: Self) {
        *self = *self | rhs;
    }
}

impl std::ops::BitAndAssign for Tristate {
    fn bitand_assign(&mut self, rhs: Self) {
        *self = *self & rhs;
    }
}
impl std::ops::Not for Tristate {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            Self::True => Self::False,
            Self::False => Self::True,
            Self::Maybe => Self::Maybe,
        }
    }
}
