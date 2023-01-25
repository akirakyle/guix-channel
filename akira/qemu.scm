(define-module (akira qemu)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cluster)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages containers)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages debian)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages figlet)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-crypto)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))


(define-public my-qemu
  (package
    (name "qemu")
    (version "7.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.qemu.org/qemu-"
                           version ".tar.xz"))
       (sha256
        (base32
         "0mr1xd78bgp1l61281sdx0338ji0aa68j2p9994sskblhwkcwjav"))
       (patches (search-patches "qemu-build-info-manual.patch"
                                "qemu-disable-aarch64-migration-test.patch"
                                "qemu-fix-agent-paths.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled code that we provide externally.
           ;; TODO: Unbundle SeaBIOS!
           (for-each delete-file-recursively
                     '("dtc" "meson"))))))
    (outputs '("out" "static" "doc"))   ;5.3 MiB of HTML docs
    (build-system gnu-build-system)
    (arguments
     (list
      ;; FIXME: Disable tests on i686 to work around
      ;; <https://bugs.gnu.org/40527>.
      #:tests? (or (%current-target-system)
                   (not (string=? "i686-linux" (%current-system))))
      #:configure-flags
      #~(let ((gcc (search-input-file %build-inputs "/bin/gcc"))
              (out #$output))
          (list (string-append "--cc=" gcc)
                ;; Some architectures insist on using HOST_CC.
                (string-append "--host-cc=" gcc)
                (string-append "--prefix=" out)
                "--sysconfdir=/etc"
                (string-append "--meson=" (search-input-file %build-inputs
                                                             "bin/meson"))
                "--enable-fdt=system"
                (string-append "--smbd=" out "/libexec/samba-wrapper")
                "--disable-debug-info"  ;for space considerations
                ;; The binaries need to be linked against -lrt.
                (string-append "--extra-ldflags=-lrt")))
      ;; Make build and test output verbose to facilitate investigation upon failure.
      #:make-flags #~'("V=1")
      #:modules `((srfi srfi-1)
                  (srfi srfi-26)
                  (ice-9 ftw)
                  (ice-9 match)
                  ,@%gnu-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'extend-test-time-outs
            (lambda _
              ;; These tests can time out on heavily-loaded and/or slow storage.
              (substitute* (cons* "tests/qemu-iotests/common.qemu"
                                  (find-files "tests/qemu-iotests" "^[0-9]+$"))
                (("QEMU_COMM_TIMEOUT=[0-9]+" match)
                 (string-append match "9")))))
          (add-after 'unpack 'disable-unusable-tests
            (lambda _
              (substitute* "tests/unit/meson.build"
                ;; Comment out the test-qga test, which needs /sys and
                ;; fails within the build environment.
                (("tests.*test-qga.*$" all)
                 (string-append "# " all))
                ;; Comment out the test-char test, which needs networking and
                ;; fails within the build environment.
                ((".*'test-char':.*" all)
                 (string-append "# " all)))))
          #$@(if (target-riscv64?)
                 '((add-after 'unpack 'disable-some-tests
                     (lambda _
                       ;; qemu.qmp.QMPConnectError:
                       ;; Unexpected empty reply from server
                       (delete-file "tests/qemu-iotests/040")
                       (delete-file "tests/qemu-iotests/041")
                       (delete-file "tests/qemu-iotests/256")

                       ;; No 'PCI' bus found for device 'virtio-scsi-pci'
                       (delete-file "tests/qemu-iotests/127")
                       (delete-file "tests/qemu-iotests/267"))))
                 '())
          (add-after 'patch-source-shebangs 'patch-embedded-shebangs
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              ;; Ensure the executables created by these source files reference
              ;; /bin/sh from the store so they work inside the build container.
              (substitute* '("block/cloop.c" "migration/exec.c"
                             "net/tap.c" "tests/qtest/libqtest.c"
                             "tests/qtest/vhost-user-blk-test.c")
                (("/bin/sh") (search-input-file inputs "/bin/sh")))
              (substitute* "tests/qemu-iotests/testenv.py"
                (("#!/usr/bin/env python3")
                 (string-append "#!" (search-input-file (or native-inputs inputs)
                                                        "/bin/python3"))))))
          (add-before 'configure 'fix-optionrom-makefile
            (lambda _
              ;; Work around the inability of the rules defined in this
              ;; Makefile to locate the firmware files (e.g.: No rule to make
              ;; target 'multiboot.bin') by extending the VPATH.
              (substitute* "pc-bios/optionrom/Makefile"
                (("^VPATH = \\$\\(SRC_DIR\\)")
                 "VPATH = $(SRC_DIR):$(TOPSRC_DIR)/pc-bios"))))
          ;; XXX ./configure is being re-run at beginning of build phase...
          (replace 'configure
            (lambda* (#:key inputs configure-flags #:allow-other-keys)
              ;; The `configure' script doesn't understand some of the
              ;; GNU options.  Thus, add a new phase that's compatible.
              (setenv "SHELL" (which "bash"))
              ;; Ensure config.status gets the correct shebang off the bat.
              ;; The build system gets confused if we change it later and
              ;; attempts to re-run the whole configuration, and fails.
              (substitute* "configure"
                (("#!/bin/sh")
                 (string-append "#!" (which "sh"))))
              (mkdir-p "b/qemu")
              (chdir "b/qemu")
              (apply invoke "../../configure" configure-flags)))
          ;; Configure, build and install QEMU user-emulation static binaries.
          (add-after 'configure 'configure-user-static
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((static (assoc-ref outputs "static"))
                     (gcc (search-input-file inputs "/bin/gcc"))
                     ;; This is the common set of configure flags; it is
                     ;; duplicated here to isolate this phase from manipulations
                     ;; to the #:configure-flags build argument, as done in
                     ;; derived packages such as qemu-minimal.
                     (configure-flags (list (string-append "--cc=" gcc)
                                            (string-append "--host-cc=" gcc)
                                            "--sysconfdir=/etc"
                                            "--disable-debug-info")))
              (mkdir-p "../user-static")
              (with-directory-excursion "../user-static"
                (apply invoke "../../configure"
                       "--static"
                       "--disable-docs" ;already built
                       "--disable-system"
                       "--enable-linux-user"
                       (string-append "--prefix=" static)
                       configure-flags)))))
          (add-after 'build 'build-user-static
            (lambda args
              (with-directory-excursion "../user-static"
                (apply (assoc-ref %standard-phases 'build) args))))
          (add-after 'install 'install-user-static
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((static (assoc-ref outputs "static"))
                     (bin (string-append static "/bin")))
                (with-directory-excursion "../user-static"
                  (for-each (cut install-file <> bin)
                            (append-map (cut find-files <> "^qemu-" #:stat stat)
                                        (scandir "."
                                                 (cut string-suffix?
                                                      "-linux-user" <>))))))))
          ;; Create a wrapper for Samba. This allows QEMU to use Samba without
          ;; pulling it in as an input. Note that you need to explicitly install
          ;; Samba in your Guix profile for Samba support.
          (add-after 'install 'create-samba-wrapper
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((libexec (string-append #$output "/libexec")))
                (call-with-output-file "samba-wrapper"
                  (lambda (port)
                    (format port "#!/bin/sh
exec smbd $@")))
                (chmod "samba-wrapper" #o755)
                (install-file "samba-wrapper" libexec))))
          (add-after 'install 'move-html-doc
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out #$output)
                     (doc #$output:doc)
                     (qemu-doc (string-append doc "/share/doc/qemu-"
                                              #$(package-version this-package))))
                (mkdir-p qemu-doc)
                (rename-file (string-append out "/share/doc/qemu")
                             (string-append qemu-doc "/html"))))))))
    (inputs
     (list alsa-lib
           bash-minimal
           dtc
           glib
           gtk+
           libaio
           libcacard                    ;smartcard support
           attr libcap-ng               ;VirtFS support
           libdrm
           libepoxy
           libjpeg-turbo
           libpng
           libseccomp
           libslirp
           liburing
           libusb                       ;USB pass-through support
           mesa
           ncurses
           ;; ("pciutils" ,pciutils)
           pixman
           pulseaudio
           sdl2
           spice
           usbredir
           util-linux
           vde2
           virglrenderer

           ;; Formats to support for .qcow2 (and possibly other) compression.
           zlib
           `(,zstd "lib")))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ;gtester, etc.
           perl
           flex
           bison
           meson-0.63
           ninja
           pkg-config
           python-wrapper
           python-sphinx
           python-sphinx-rtd-theme
           texinfo
           ;; The following static libraries are required to build
           ;; the static output of QEMU.
           `(,glib "static")
           `(,pcre "static")
           `(,zlib "static")))
    (home-page "https://www.qemu.org")
    (synopsis "Machine emulator and virtualizer")
    (description
     "QEMU is a generic machine emulator and virtualizer.

When used as a machine emulator, QEMU can run OSes and programs made for one
machine (e.g. an ARM board) on a different machine---e.g., your own PC.  By
using dynamic translation, it achieves very good performance.

When used as a virtualizer, QEMU achieves near native performances by
executing the guest code directly on the host CPU.  QEMU supports
virtualization when executing under the Xen hypervisor or using
the KVM kernel module in Linux.  When using KVM, QEMU can virtualize x86,
server and embedded PowerPC, and S390 guests.")

    ;; Many files are GPLv2+, but some are GPLv2-only---e.g., `memory.c'.
    (license license:gpl2)

    ;; Several tests fail on MIPS; see <http://hydra.gnu.org/build/117914>.
    (supported-systems (fold delete %supported-systems
                             '("mips64el-linux" "i586-gnu")))))
(define-public my-qemu-minimal
  ;; QEMU without GUI support, only supporting the host's architecture
  (package
    (inherit my-qemu)
    (name "qemu-minimal")
    (outputs '("out" "doc"))
    (synopsis
     "Machine emulator and virtualizer (without GUI) for the host architecture")
    (arguments
     (substitute-keyword-arguments (package-arguments my-qemu)
       ((#:configure-flags configure-flags #~'())
        ;; Restrict to the host's architecture.
        (let* ((system (or (%current-target-system)
                           (%current-system)))
               (target-list-arg
                (match system
                  ((? (cut string-prefix? "i686" <>))
                   "--target-list=i386-softmmu")
                  ((? (cut string-prefix? "x86_64" <>))
                   "--target-list=i386-softmmu,x86_64-softmmu")
                  ((? (cut string-prefix? "mips64" <>))
                   (string-append "--target-list=mips-softmmu,mipsel-softmmu,"
                                  "mips64-softmmu,mips64el-softmmu"))
                  ((? (cut string-prefix? "mips" <>))
                   "--target-list=mips-softmmu,mipsel-softmmu")
                  ((? (cut string-prefix? "aarch64" <>))
                   "--target-list=arm-softmmu,aarch64-softmmu")
                  ((? (cut string-prefix? "arm" <>))
                   "--target-list=arm-softmmu")
                  ((? (cut string-prefix? "alpha" <>))
                   "--target-list=alpha-softmmu")
                  ((? (cut string-prefix? "powerpc64" <>))
                   "--target-list=ppc-softmmu,ppc64-softmmu")
                  ((? (cut string-prefix? "powerpc" <>))
                   "--target-list=ppc-softmmu")
                  ((? (cut string-prefix? "s390" <>))
                   "--target-list=s390x-softmmu")
                  ((? (cut string-prefix? "riscv" <>))
                   "--target-list=riscv32-softmmu,riscv64-softmmu")
                  (else       ; An empty list actually builds all the targets.
                   '()))))
          #~(cons #$target-list-arg #$configure-flags)))
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'configure-user-static)
            (delete 'build-user-static)
            (delete 'install-user-static)))))

    ;; Remove dependencies on optional libraries, notably GUI libraries.
    (native-inputs (filter (lambda (input)
                             (match input
                               ;; Work around the fact that modify-inputs can not
                               ;; delete specific outputs; i.e. here we should keep
                               ;; `(,glib "bin"), but not `(,glib "static").
                               ((label package output)
                                (not (string=? "static" output)))
                               (_ input)))
                           (modify-inputs (package-native-inputs my-qemu)
                             (delete "gettext-minimal"))))
    (inputs (modify-inputs (package-inputs my-qemu)
              (delete "libusb"
                      "mesa"
                      "sdl2"
                      "spice"
                      "virglrenderer"
                      "gtk+"
                      "usbredir"
                      "libdrm"
                      "libepoxy"
                      "pulseaudio"
                      "vde2"
                      "libcacard")))))
