## Build instructions

To build clancs, simply `cd` to the clancs directory and run the following:

    cd pyclang/src/
    mkdir build
    cd build
    cmake ..
    make

This will build the latest (trunk) version of LLVM and Clang. To build the 3.3 stable release instead, run cmake as:

    cmake .. -DCLANG_VERSION=3.3

To build on Windows, you will need [MinGW-w64](mingw-w64.sourceforge.net)

## Usage

First get the latest version of clancs in a directory of your choice:

    git clone https://github.com/ameyp/clancs.git ~/.emacs.d/packages/clancs --recursive

Add the directory to your load-path. For example:

    (add-to-list 'load-path "~/.emacs.d/packages/clancs")

Put the following code in your init.el to have it load only when a C/C++ buffer is visited:

    (add-hook 'c-mode-common-hook
          (lambda ()
            (require 'clancs)
            (clancs-init)
            (setq ac-sources '(ac-source-clancs))))

To have clancs start up during emacs startup instead, simply put the body of the lambda function directly inside your init.el.

## Project-specific configuration

Clang needs to be able to compile your source code completely for it to show appropriate completions. Clancs currently uses [directory-local variables](http://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Local-Variables.html) to pass compilation flags to clang. As such, you will receive a warning from Emacs about the directory-local variables being unsafe every time you visit a file nested in a directory with a .dir-locals.el file. When you receive this warning, emacs will present you with the option of permanently ignoring these particular variables. I apologize for the inconvenience, but I haven't yet settled on a third-party project-management emacs package for use with clancs.

So, create a .dir-locals.el file in the root directory of the codebase for which you wish clancs to provide automatic completions. This file will contain all the CFLAGS and CXXFLAGS required for compiling any file in that codebase. For example, here are the contents of one of my .dir-locals.el:

    ((c++-mode . ((clancs-compile-flags . ("-Isrc" "-Iinclude/**"
                                           "-DVARIANT_g" "-DQNX" "-Wno-dangling-else"
                                           "-Wno-missing-declarations" "-Wno-null-conversion")))))

Note the second entry, `-Iinclude/**`. The special notation of `**` indicates that the include directory and all of its subdirectories - recursively - be added to the include path. This means that if I have a directory structure like the following:

    src/
    include/
    include/math
    include/math/vector
    include/gui
    include/gui/rocket

the `-Iinclude/**` flag will equivalently pass `-Iinclude -Iinclude/math -Iinclude/math/vector -Iinclude/gui -Iinclude/gui/rocket` to clang for compilation.

Once you've added the right compilation flags, simply visit a file in the codebase or run `revert-buffer` on the currently-open file and clancs should complete stuff for you.

## Troubleshooting

1. I'm seeing the following error upon initializing clancs:
    File mode specification error: (error "Server may raise an error. Use \"M-x epc:pop-to-last-server-process-buffer RET\" to see full traceback:

Run `M-x epc:pop-to-last-server-process-buffer`. If the output contains the following line,
    OSError: libclang.so: cannot open shared object file: No such file or directory

it means that you haven't don't have a supported system installation of clang and also haven't built it yourself as outlined above. Please do that first and try again.

If the error says something else, please create a new issue and paste the full traceback there.

2. I'm not seeing any completions when I start typing

Clancs typically takes a few seconds to compile a file the first time you visit. If you still don't see any completions after about five-seven seconds, your compilation might have failed. Please check the ` *clancs output*` buffer, any compilation errors will be printed there.

## Pending

1. Since I've designed clancs to provide completions asynchronously (and since I'm relatively unexperienced at writing emacs plugins), you might run into issues if you start typing code at one location in a file and then quickly jump to a different location in the file before the list of completions appears.
2. I've come across sporadic bugs where clang compilation of a file fails and clang asserts, however this was in clang release 3.1, so I'm hoping that the trunk version doesn't have this issue. If you get completions working and then start seeing errors in the message log every time you type a key (and completions stop working), please raise an issue on this repo and provide the contents of every open emacs buffer (other than the buffers that you have manually created of course).
3. Terminate the epc processes so that the user doesn't get warnings about running processes while exiting emacs.