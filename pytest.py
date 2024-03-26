from fixpoints import *
from arpeggio import Optional, ZeroOrMore, OneOrMore, EOF, ParserPython, PTNodeVisitor, visit_parse_tree
from arpeggio import RegExMatch as _

def identifier(): return _(r'[a-zA-Z]')
def pvar(): return "?", identifier
def pnum(): return _(r'\d*')
def pwildcard(): return "_"
def pterm(): return [pvar, pnum, pwildcard]
def patom(): return identifier, "(", pterm, ZeroOrMore((",", pterm)), ")"
def ptype(): return ["str", "int"]
def pimport(): return "import", "(", OneOrMore(ptype), ")"
def prule(): return patom, Optional(":-", [pimport, patom], ZeroOrMore((",", [pimport, patom]))), "."
def pexport(): return "export", identifier, "."
def program(): return ZeroOrMore(prule), Optional(pexport), EOF

class ProgramVisitor(PTNodeVisitor):
    variables = {}
    fw = None
    bw = None

    def visit_identifier(self, node, children):
        return str(node.value)

    def visit_pvar(self, node, children):
        var_name = children[0]

        if var_name in self.variables:
            self.variables[var_name]["occurrences"].append((node.position, node.position_end))
            return var(self.variables[var_name]["index"])

        new_index = max([x["index"] for x in self.variables.values()], default=-1) + 1
        self.variables[var_name] = {
            "index": new_index,
            "occurrences": [(node.position, node.position_end)]
        }

        return var(new_index)

    def visit_pwildcard(self, node, chlildren):
        return wildcard()

    def visit_pnum(self, node, children):
        return int(node.value)

    def visit_patom(self, node, children):
        pred, *terms = children
        return atom(pred, terms)

    def visit_ptype(self, node, children):
        if node.value == "str":
            return str
        elif node.value == "int":
            return int
        else:
            raise f"Unsupported type: {node.value}"

    def visit_pimport(self, node, children):
        return import_data(*children)

    def visit_prule(self, node, children):
        var_map = dict(self.variables)
        self.variables.clear()
        head, *body = children
        return {
            "variables": var_map,
            "head": head,
            "body": body,
            "flat": input[node.position:node.position_end],
            "position": (node.position, node.position_end)
        }
    
    def visit_pexport(self, node, children):
        return { "export": children[0] }

    def second_prule(self, result):
        if result["body"] == []:
            return

        typ_info, fails = execute_body(self.fw, result["body"])
        for fail in fails:
            if type(fail) == MeetFailure:
                print(f'incompatible types in rule {result["flat"]}')

    def visit_program(self, node, children):
        print("Performing analysis...")

        if not "export" in children[-1]:
            print("No exported predicates - Nothing to do.")
            return
        
        exported = children[-1]["export"]
        rules = [rule(r["head"], r["body"]) for r in children[:-1]]
        self.fw, self.bw = inference(rules, exported)



rule_parser = ParserPython(program)

input = """
b(4, 1).

a(1, ?m) :- b(_, ?m).

n(?x) :- import(str).
c(?x) :- a(?x, _), n(?x).

c(?x) :- import(int).
c(?x) :- a(?x, ?x).

export c.
"""

parse_tree = rule_parser.parse(input)
visit_parse_tree(parse_tree, ProgramVisitor())

# for rule in result:
#     print(rule)

# rules = [
#     rule(atom("data", [var(0), var(1)]), [import_data(int, str)])
# ]
# 
# fw, bw = inference(rules, "data")
# 
# body_subst = execute_body(fw, [atom("data", [var(0), var(0)]), atom("data", [var(1), wildcard()])])
# 
# print(body_subst)
# 
# body_subst, failures = execute_body(fw, [atom("data", [functor("f", [var(0)]), wildcard()])])
# 
# # print(("Test", execute_body(fw, [atom("data", [var(0), 1])])))
# 
# for failure in failures:
#     if type(failure) is DeconstructFailure:
#         print("Deconstruct failure happened")
#         print(failures)
# 
# print(body_subst)

