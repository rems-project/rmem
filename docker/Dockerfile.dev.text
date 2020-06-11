FROM rmem/base-dev as intermediate

SHELL ["/bin/bash", "-o", "pipefail", "-ceu"]

ARG GUEST_UID=1000
ARG GUEST_GID=${GUEST_UID}
ARG GUEST_USER=rems
ARG GUEST_GROUP=${GUEST_USER}

ARG MODE="opt"
ARG ISA="PPCGEN,AArch64,RISCV"

# for this COPY to work the context must be rmem/
# e.g. build with `docker build -f- path/to/rmem < Dockerfile.local`
# we also need to own the dir
COPY --chown=${GUEST_UID}:${GUEST_GID} . /home/rems/rmem-local/

# and finally build rmem for real
RUN cd rmem-local/ \
    && eval $(opam env) \
    && opam install . --deps-only -y

RUN eval $(opam env) \
    && ulimit -s unlimited \
    && make -C rmem-local MODE=${MODE} UI=text ISA=${ISA} \
    && cp rmem-local/rmem bin/ \
    && echo 'export ISA_DEFS_PATH="$HOME/rmem-local/"' >> .profile

# If you want a small image that can run rmem you can copy rmem/*.defs to
# ~/bin/ and remove all the folders in ~/, except ~/bin/ and ~/.opam/. You
# can also remove most of the apt and opam packages, but I did not check which
# exactly.

# Now we pull in the litmus-tests
COPY --from=litmus-tests --chown=${GUEST_UID}:${GUEST_GID} /home/${GUEST_USER} /home/${GUEST_USER}/