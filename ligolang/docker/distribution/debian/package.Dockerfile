ARG targetBuildImage
FROM ${targetBuildImage}
ARG version

RUN apt-get update -qq
RUN apt-get -y -qq install \
    dput \
    devscripts

RUN mkdir /package && mkdir /package/bin && mkdir /package/DEBIAN && mkdir /package/dist
# @TODO: make the binary path configurable
RUN cp /home/opam/.opam/4.09/bin/ligo /package/bin/ligo

# @TODO: inherit version (and other details) from the ligo opam package definition
# In our case we're using the version field to name our package accordingly,
# however this is most likely not ideal
# Also, the architecture field should not be 'all' but rather specific instead.
RUN echo "Package: ligo\n\
Version: $version\n\
Architecture: all\n\
Maintainer: info@ligolang.org\n\
Depends: libev4, libgmp10, libgmpxx4ldbl\n\
Homepage: http://ligolang.org\n\
Description: LIGO is a statically typed high-level smart-contract language that compiles down to Michelson." >> /package/DEBIAN/control

RUN cd /package/dist && dpkg-deb --build /package .

# Test the package
# Optionally this could/should be done in a more pristine docker environment - in a separate image
RUN apt-get update -qq
RUN apt-get -y -f install "/package/dist/$(ls /package/dist)"
RUN ligo --help