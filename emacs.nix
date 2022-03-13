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
        src = let
          rev = "5ba9c8c364f652221a0ac9ed918a831e122581db";
        in builtins.fetchTarball {
          url = "https://git.savannah.gnu.org/cgit/emacs.git/snapshot/emacs-${rev}.tar.gz";
          sha256 = "0gh2qqcsmjych3xazq5n6s430yfnc5k1hmc7vin6c2scnlswlbz0";
        };
        version = "28.0.92";
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

  withPatches = drv: patches: drv.overrideAttrs (o: { inherit patches; });

  emacsWithPackages = (emacsPackagesNgGen emacs').emacsWithPackages;

in emacsWithPackages(epkgs:

  [
    defaultEl
  ]

  ++

  (with epkgs.elpaPackages; [
    company
    counsel
    ivy
    modus-themes
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
