{ pkgs }:

let

  inherit (pkgs)
    runCommand
    xsel
    emacsPackagesNgGen;

  clhs = runCommand
    "clhs"
    {
      src = fetchTarball {
        url = https://galkowski.xyz/HyperSpec-7-0.tar.gz;
        sha256 = "1zsi35245m5sfb862ibzy0pzlph48wvlggnqanymhgqkpa1v20ak";
      };
    }
    "cp -Tr $src $out";

  defaultEl = (runCommand "default.el" { inherit clhs; } ''
      mkdir -p $out/share/emacs/site-lisp
      cp -v ${./default.el} $out/share/emacs/site-lisp/default.el
      substituteAllInPlace $out/share/emacs/site-lisp/default.el
    '');

  emacs' =
    (pkgs.emacs.override { nativeComp = true; srcRepo = true; })
      .overrideAttrs (o: {
        src = builtins.fetchTarball {
          url = "https://git.savannah.gnu.org/cgit/emacs.git/snapshot/emacs-28.1.tar.gz";
          sha256 = "01mfwl6lh79f9icrfw07dns3g0nqwc06k6fm3gr45iv1bjgg0z8g";
        };
        version = "28.1";
        patches = [ ./patches/tramp-detect-wrapped-gvfsd.patch ] ;
        meta = o.meta // { mainProgram = "emacs"; };
      });

  emacsPackages' = pkgs.emacsPackagesFor emacs';

  build-elisp-package = args:
    emacsPackages'.trivialBuild args;

  modus-themes = build-elisp-package {
    pname = "modus-themes";
    src = fetchTarball {
      url = https://galkowski.xyz/modus-themes-2.0.0.tar;
      sha256 = "122xg6wk2mn1c69kaqkqkgqkbw61n13x3ylwf5q2b2kr60skn1zh";
    };
  };

  efsl = build-elisp-package {
    pname = "efsl";
    src = fetchTarball {
      url = "https://fossil.galkowski.xyz/efsl/tarball/266c0f94f70aae6a/efsl.tgz";
      sha256 = "05zlmmrqffcs4azyw9lq95i7k6zqb947iz2mvgnqfzi15zn63lk9";
    };
  };

  withPatches = drv: patches: drv.overrideAttrs (o: { inherit patches; });

  emacsWithPackages = (emacsPackagesNgGen emacs').emacsWithPackages;

in emacsWithPackages(epkgs:

  let
    inherit (epkgs.melpaPackages) cider direnv flycheck slime;
    cider' = (withPatches cider [ ./patches/cider-return-buffer-in-switch-to-repl-buffer.patch ]);
    direnv' = (withPatches direnv [ ./patches/direnv-el-message-not-warning.patch ]);
    flycheck' = (withPatches flycheck [ ./patches/flycheck-dont-message-suspicious.patch ]);
    slime' = (withPatches slime [ ./patches/slime-cl-indent-other-braces.patch ]);
  in
    
  [
    defaultEl
    efsl
    cider'
    direnv'
    flycheck'
    slime'
  ]

  ++

  (with epkgs.elpaPackages; [
    company
    counsel
    ivy
    which-key
  ])

  ++

  (with epkgs.melpaPackages; [
    anzu
    diminish
    editorconfig
    evil
    evil-anzu
    evil-matchit
    glsl-mode
    magit
    nix-mode
    rg
    slime-company
    use-package
    vc-fossil
    wgrep
    winum
    yaml-mode
  ])

)
