.PHONY: all clean

all: flathead.native

SRCS := compression.ml deque.ml dictionary.ml \
    evaluation_stack.ml flathead.ml frame.ml frameset.ml globals.ml \
    iff.ml immutable_bytes.ml instruction.ml interpreter.ml local_store.ml \
    object.ml quetzal.ml randomness.ml reachability.ml \
    routine.ml screen.ml status_line.ml story.ml tokeniser.ml transcript.ml \
    type.ml utility.ml window.ml zstring.ml runner.ml

flathead.native: $(SRCS)
	ocamlbuild -use-ocamlfind $@

clean:
	ocamlbuild -clean
