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

* Lift ships a new release every six months, with three milestones and at least one release
  candidate building up to each release. Each normal release results in a bump of the middle number
  of the version (e.g. 3.1.0 to 3.2.0).
* Brand new features are always a part of new releases. API-incompatible changes will only ship as a
  part of a major release, which results in a bump of the first number of the version (e.g. 2.6.0 to
  3.0.0).
* Bug fixes and minor performance improvements can, by consensus of the Committers, be shipped for
  releases for 18 months after their release date.
* Security fixes will be shipped for the current and previous two major releases.
  * Due to a change in our release scheme, Lift 2.5 will receive security fixes until the release
  of Lift 4.0 and Lift 2.6 will receive security fixes until the release of Lift 5.0.
  * Any application using Lift 2.4 or earlier should be upgraded as soon as possible.
  * Major releases aren't on a regular schedule, but we will announce and update this policy with
  actual dates when major releases become planned.

As of June 2nd, 2018 the current support status looks like this:

|Version  |Initial Release  | Last Release     | Current Support Status          |
|---------|-----------------|------------------|---------------------------------|
|< 2.5    |                 |                  | Not supported                   |
|2.5      |June 2013        | 2.5.4 (Jan 2016) | Security fixes (until Lift 4.0) |
|2.6      |January 2015     | 2.6.3 (Jan 2016) | Security fixes (until Lift 5.0) |
|3.0      |November 2016    | 3.0.2 (Sep 2017) | Security fixes (until Lift 6.0) |
|3.1      |July 2017        | 3.1.1 (Sep 2017) | Minor fixes (until Jan 2019)    |
|3.2      |January 2018     | 3.2.0 (Jan 2018) | Minor fixes (until July 2019)   |
|3.3      |July 2018 (est)  | Pre-release      | Active development              |

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

### I need to plan for longer-term support of a Lift application

For some organizations 18 months just isn't a sufficient support window. If your organization is in
this situation, please consider reaching out on the [Mailing List][ml]. You may be able to hire a
Lift community member to help you provide longer-term support for your application.

[ml]: https://groups.google.com/forum/#!forum/liftweb
