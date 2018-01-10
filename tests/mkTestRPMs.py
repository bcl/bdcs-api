#!/usr/bin/python3
# Copyright (C) 2017 Red Hat, Inc.
#
# This file is part of bdcs-api.
#
# bdcs-api is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# bdcs-api is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with bdcs-api.  If not, see <http://www.gnu.org/licenses/>.

# This utility is used to create the fake rpms used for ServerSpec.h API testing
# It depends on rpmfluff from https://pagure.io/rpmfluff

from contextlib import contextmanager
import os
import shutil
import tempfile

from rpmfluff import SimpleRpmBuild, expectedArch

# helpers for creating RPMs to test with
@contextmanager
def in_tempdir(prefix='tmp'):
    oldcwd = os.getcwd()
    tmpdir = tempfile.mkdtemp(prefix=prefix)
    os.chdir(tmpdir)
    yield
    os.chdir(oldcwd)
    shutil.rmtree(tmpdir)

def make_fake_rpm(outdir, name, epoch, version, release, requires=None, provides=None):
    p = SimpleRpmBuild(name, version, release)
    if epoch:
        p.epoch = epoch
    if requires:
        p.add_requires(requires)
    if provides:
        p.add_provides(provides)

    with in_tempdir("bdcs-test-rpms."):
        p.make()
        rpmfile = p.get_built_rpm(expectedArch)
        shutil.move(rpmfile, os.path.join(outdir, os.path.basename(rpmfile)))


def main(outdir):
    # Build 4 test packages for use with the API ServerSpec.hs tests
    pkgs = [["bdcs-fake-bart",  0, "1.3.1", "12", "bdcs-fake-homer", None],
            ["bdcs-fake-lisa",  3, "1.0.0", "1",  "music", None],
            ["bdcs-fake-homer", 0, "2.0.1", "4",  None, None],
            ["bdcs-fake-sax",   0, "3.8.1", "1",  None, "music"],
           ]
    for args in pkgs:
        make_fake_rpm(outdir, *args)

if __name__ == "__main__":
    main(os.path.abspath("./tests/mddb/"))
