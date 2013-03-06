from pyepc.server import EPCServer
from pyclang import sublimeclang, sublime

server = EPCServer(('localhost', 0))
scaa = sublimeclang.SublimeClangAutoComplete()

@server.register_function
def query_completions(filename, position, prefix, flags, content=""):
    print filename, position, prefix
    if content != "":
        view = sublime.View(filename, position, flags, content)
    else:
        view = sublime.View(filename, position, flags)
    return scaa.on_query_completions(view, prefix, [position])

server.print_port()
server.serve_forever()
