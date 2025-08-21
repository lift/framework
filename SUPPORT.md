# The Lift Framework Support Policy

Ongoing maintenance and development of the Lift Framework is a volunteer effort executed by a
passionate group of Lift supporters known as the Lift Committers. With the assistance of
outside contributors, the Lift Committers work to ensure that with each release of Lift we provide
a selection of new features and bug fixes. The Lift Committers stand behind and provide ongoing
support for the contributions they make to Lift, and a condition of accepting changes from outside
contributors is that the Committers feel comfortable supporting those changes long term.

Because maintenance of the Framework is a  volunteer effort and all of the Committers have busy
lives with precious free time, they must be intentional with the community support they provide for
Lift. To that end, the Committers have decided to embrace the following support policy for releases
of Lift:

* Brand new features are always a part of new releases. API-incompatible changes will only ship as a
  part of a major release, which results in a bump of the first number of the version (e.g. 2.6.0 to
  3.0.0).
* Bug fixes and minor performance improvements can, by consensus of the Committers, be shipped for
  releases for 18 months after their release date.
* Security fixes will be shipped for previous two minor releases until a new major release is made
  at which point the last minor release of the previous major will receive updates for six months
  after the release of the most recent major.

As of August 20th, 2025 the current support status looks like this:

|Version  |Initial Release  | Last Release     | Current Support Status          |
|---------|-----------------|------------------|---------------------------------|
|< 3.4    |                 |                  | Not supported                   |
|3.4      |October 2019     | 3.4.3 (Nov 2020) | Security fixes only             |
|3.5      |October 2021     | 3.5.0 (Oct 2021) | Security fixes only             |
|4.0      |Not yet released | N/A              | Active development              |
|5.0      |Not yet released | N/A              | Subsequent release              |

Per this support policy please ensure you're using a currently supported version of Lift before:

* Opening a [Mailing List][ml] thread asking for help from the committers.
* Filing an issue in GitHub.
* Opening a Pull Request intended to patch an older version of Lift.

## Common issues

We're sure that this support policy won't meet every organization's needs. To that end, the
committers wanted to address a few specific problems that may come up as a result of this support
strategy:

### I need help upgrading Lift

If you're using an ancient version of Lift, you may find that a lot has changed and upgrading has
become quite challenging. If you're in this position, please reach out to the community on the
[Lift Mailing List][ml]. We're happy to answer quick questions about changes we've made.

If your needs are more involved, you may want to consider hiring a member of the Lift community
or a Lift Committer to help you upgrade your application. A number of members in the community do
part-time consulting off and on and several more consult full time as their day job.

[ml]: https://groups.google.com/forum/#!forum/liftweb
