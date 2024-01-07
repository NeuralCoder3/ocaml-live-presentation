FROM ocaml/opam

RUN sudo apt install -y libglpk-dev z3 python3

RUN opam pin lp --dev-repo && \
    opam pin lp-glpk --dev-repo && \
    opam install lp lp-glpk && \
    opam pin lp-glpk-js --dev-repo && \
    opam install lp-glpk-js
# RUN opam install z3

RUN opam install ocaml-lsp-server ocamlformat merlin
RUN opam user-setup install

RUN opam install base batteries zarith
RUN opam install utop menhir ppx_deriving

RUN opam switch create 5.0.0 && eval $(opam env)

RUN sudo apt update
RUN sudo apt install --yes pkg-config libgles2-mesa-dev libffi-dev libsdl2-dev

RUN opam install gg result grenier conf-gles2 stb_image stb_truetype menhir dune utop tgls tsdl
RUN opam pin add hotcaml https://github.com/let-def/hotcaml.git

