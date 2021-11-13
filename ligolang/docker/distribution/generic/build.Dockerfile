ARG target
FROM ocaml/opam2:${target}

ARG ci_job_id
ARG ci_commit_sha
ARG commit_date
ENV CI_JOB_ID=$ci_job_id
ENV CI_COMMIT_SHA=$ci_commit_sha
ENV COMMIT_DATE=$commit_date

RUN opam switch 4.09 && eval $(opam env)

USER root

# Add contents of the current directory to /ligo where it can be
# accessed when building the image.
#
# This is useful when building either locally, or on the CI
# because the currently checkout out version (from git) will be used
# to build the image
ADD . /ligo
# Set the current working directory to /ligo for
# the upcoming scripts
WORKDIR /ligo

# Install required native dependencies
RUN sh scripts/install_native_dependencies.sh

RUN opam update

# Install ligo
RUN sh scripts/install_opam_deps.sh
RUN opam install -y . || (tail -n +1 ~/.opam/log/* ; false)

# Use the ligo binary as a default command
ENTRYPOINT [ "/home/opam/.opam/4.09/bin/ligo" ]
