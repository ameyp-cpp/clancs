from pyepc.server import EPCServer
from pyclang import sublimeclang, sublime

server = EPCServer(('localhost', 0))
scaa = sublimeclang.SublimeClangAutoComplete()

@server.register_function
def query_completions(filename, position, prefix, flags, tmp_file=None):
    print filename, position, prefix, flags
    view = sublime.View(filename, position, flags, tmp_file)
    return scaa.on_query_completions(view, prefix, [position])

server.print_port()
server.serve_forever()
