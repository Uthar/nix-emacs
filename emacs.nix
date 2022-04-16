{ pkgs }:

let

  inherit (pkgs)
    runCommand
    xsel
    emacsPackagesNgGen;

  # Include the HyperSpec in the Emacs closure to make offline browsing via
  # slime-hyperspec-lookup possible.
  clhs = runCommand
    "clhs"
    {
      src = fetchTarball {
        url = https://galkowski.xyz/HyperSpec-7-0.tar.gz;
        sha256 = "1zsi35245m5sfb862ibzy0pzlph48wvlggnqanymhgqkpa1v20ak";
      };
    }
    "cp -Tr $src $out";

  # The Emacs program itself.
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

  defaultEl = (runCommand "default.el" { inherit clhs; } ''
    mkdir -p $out/share/emacs/site-lisp
    cp -v ${./default.el} $out/share/emacs/site-lisp/default.el
    substituteAllInPlace $out/share/emacs/site-lisp/default.el
  '');

  # defaultEl = build-elisp-package {
  #   pname = "default.el";
  #   src = ./default.el;
  #   packageRequires = [ emacsPackages'.use-package ];
  # };

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
  with epkgs.melpaPackages;
  with epkgs.elpaPackages;
  let
    direnv' = withPatches direnv [ ./patches/direnv-el-message-not-warning.patch ];
    flycheck' = withPatches flycheck [ ./patches/flycheck-dont-message-suspicious.patch ];
    slime' = withPatches slime [ ./patches/slime-cl-indent-other-braces.patch ];
    cider' = withPatches cider [ ./patches/cider-return-buffer-in-switch-to-repl-buffer.patch ];
  in
    [

    # The actual elisp code that Emacs will run on startup.
    defaultEl

    # Ivy is easier to use than the built-in completion, and is not super heavy.
    # Counsel replaces a bunch of built in interaction functions to use Ivy.
    ivy
    counsel

    # Convenience for having completion drop-down menus.
    company

    # Convenience for visually displaying compiler warnings in the source code.
    flycheck'

    # Convenience package for showing available key bindings.
    which-key

    # Ag and Ripgrep are external programs, but are tremendously more performant
    # than the built-in elisp utilities, which makes the hassle worth it.
    ag
    rg

    # Direnv is a convenience utility for having per-directory shell hooks.
    # This package simply makes that work with Emacs' own find-file.
    direnv'

    # Support for .editorconfig files.
    # Not strictly necessary, but useful for polyglot projects.
    editorconfig

    # This library makes it possible to hide minor modes from the mode line.
    # it's useful to make me forget how much crap I am actually running.
    diminish

    # Minor visual nicety: show form feed characters as vertical lines.
    # This is a buggy package: the display often breaks by overflowing to the
    # next line.
    # I should replace this with a simple font-lock configuration instead.
    page-break-lines

    # Addon to grep mode that allows editing and saving the matches.
    # Analogous to the built-in wdired mode.
    wgrep

    # Packages for emulating Vim
    anzu
    evil-anzu
    evil
    evil-matchit
    evil-surround
    evil-collection

    # Convenience modes for working with programming languages.
    # Syntax highlighting, completion, indentation etc.
    # In some cases even crazy things like debuggers and such.
    cider'
    glsl-mode
    go-mode
    groovy-mode
    lsp-mode
    lsp-python-ms
    nix-mode
    slime'
    slime-company
    terraform-mode
    yaml-mode

    # Version control
    # Subjectively nicer than the built-in vc-mode for some use cases.
    # One advantage of these is the displaying of a single status buffer, which
    # provides a lot of information in one place.
    efsl
    magit

    # A macro for organizing code in the default.el file.
    # use-package

    # Minor convenience - display window numbers in the mode line.
    # Primarily useful for fast switching via numbered keyboard shortcuts.
    winum

    # Very small package that simply adds a function to switch between the
    # built-in dark and light modus themes - modus-operandi and modus-vivendi.
    modus-themes

    # Utilities for managing "projects", which are simply explicitly named
    # directories.
    projectile

  ]
)
