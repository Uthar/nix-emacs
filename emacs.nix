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
      substitute ${./default.el} $out/share/emacs/site-lisp/default.el \
        --replace xsel ${xsel}/bin/xsel
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

  build-elisp-package = { name, src }:
    runCommand name {} ''
        mkdir -p $out/share/emacs/site-lisp
        cp -Tr ${src} $out/share/emacs/site-lisp
      '';

  modus-themes = build-elisp-package {
    name = "modus-themes";
    src = fetchTarball {
      url = https://galkowski.xyz/modus-themes-2.0.0.tar;
      sha256 = "122xg6wk2mn1c69kaqkqkgqkbw61n13x3ylwf5q2b2kr60skn1zh";
    };
  };

  efsl = build-elisp-package {
    name = "efsl";
    src = fetchTarball {
      url = "https://fossil.galkowski.xyz/efsl/tarball/7dc9d6e668036392/efsl.tgz";
      sha256 = "182p5mgbnjvylh347cmb1ssngpzcf556wd7w48zab4rq1n158jpw";
    };
  };

  withPatches = drv: patches: drv.overrideAttrs (o: { inherit patches; });

  emacsWithPackages = (emacsPackagesNgGen emacs').emacsWithPackages;

in emacsWithPackages(epkgs:

  [
    defaultEl
    modus-themes
    efsl
  ]

  ++

  (with epkgs.elpaPackages; [
    company
    counsel
    ivy
    undo-tree
    which-key
  ])

  ++

  (with epkgs.melpaPackages; [
    ag
    anzu
    browse-kill-ring
    (withPatches cider [ ./patches/cider-return-buffer-in-switch-to-repl-buffer.patch ])
    company-terraform
    diminish
    (withPatches direnv [ ./patches/direnv-el-message-not-warning.patch ])
    editorconfig
    evil
    evil-anzu
    evil-collection
    evil-matchit
    evil-surround
    (withPatches flycheck [ ./patches/flycheck-dont-message-suspicious.patch ])
    glsl-mode
    go-mode
    groovy-mode
    hl-todo
    lsp-mode
    lsp-python-ms
    magit
    nix-mode
    page-break-lines
    paredit
    projectile
    rg
    ripgrep
    (withPatches slime [ ./patches/slime-cl-indent-other-braces.patch ])
    slime-company
    terraform-mode
    use-package
    vc-fossil
    wgrep
    winum
    yaml-mode
  ])

)
