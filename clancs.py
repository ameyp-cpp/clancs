from pyepc.server import EPCServer
from pyclang import sublimeclang, sublime

server = EPCServer(('localhost', 0))
scaa = sublimeclang.SublimeClangAutoComplete()

@server.register_function
def echo(*a):
    return a

@server.register_function
def query_completions(filename, position, prefix, flags):
    view = sublime.View(filename, position, flags[1])
    return scaa.on_query_completions(view, prefix[1], [position])

server.print_port()
server.serve_forever()
