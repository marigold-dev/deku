---
id: documentation-and-releases
title: Documentation and releases
---


## Documentation

If you'd like to contribute to the docs you can find them at `gitlab-pages/docs` in raw markdown form.
Deployment of the docs/website for LIGO is taken care of within the CI, from `dev` and `master` branches.


## Submitting your changes

### Changelog

We're using our own implementation of [GitLab's changelog model](https://docs.gitlab.com/ee/development/changelog.html).

In short, it involves the following procedures:

- Entries are located in ./changelog folder.

- They are generated with a script (./scripts/add-changelog-entry.sh), and they are YAML files with the following format:
```yaml
title: string
merge_request: int
author: string
type: added|fixed|changed|deprecated|removed|performance|other
```
Obviously, you can add those files manually too if you want.

- Any API change **must** have a changelog entry. Example: "Removed list_iter function"
- Any user-facing change **should** have a changelog entry. Example: “Improved error reporting for ReasonLIGO”
- Performance improvements **should** have a changelog entry.
- Any docs-only changes **should not** have a changelog entry.
- A fix for a regression introduced and then fixed in the same release **should not**
have a changelog entry.
- Any developer-facing change (e.g., CI, refactoring, technical debt remediation,
test suite changes) **should not** have a changelog entry. Example: “Refactor nix expressions for webide”

## Releases & versioning

### Development releases (next)

Development releases of LIGO are tagged `next` and are built with each commit to the `dev` branch. Both the docker image & the website are published automatically.

### Stable releases

Stable releases are tags of form `x.x.x`, generally sticking to semver conventions. Such tags are automatically built on CI, producing a release in https://gitlab.com/ligolang/ligo/-/releases and pushing images to docker.
