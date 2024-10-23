# IT DevCon 2024 sessions by Jeroen Wiert Pluimers

## Speaker photo

![image](https://gist.github.com/user-attachments/assets/8ccc5ea0-e1f6-49e7-91b6-ae9e3eaee8fe)

## Speaker profile

Jeroen has been staying low profile for the 5 years after he got diagnosed with rectum cancer, which after the first radiation treatment had spread to his liver.

He never really stopped practicing music - it keeps his mind in balance - nor being in IT, but had first focus ensuring the long term care for his mentally retarded brother could be performed largely without him, and his wife and he could move to a place that had an elevator to ensure it would be the place where they could live even in case they would become less mobile.

During that period he and regaining energy, he not only kept his [wiert.me](https://wiert.me/) blog up, but also informally helped quite a few people coping with IT issues and health ones, be it on the physical side, or mental side.

With sufficient energy, he now is back taking on part time remote work as a seasoned software developer / DevOps engineer which has been practicing agility for over 35 years with a large awareness of [#inclusion](https://www.google.com/search?q=%23inclusion) and [#a11y](https://www.google.com/search?q=%23a11y) which is the short form of [#accessibility](https://www.google.com/search?q=%23accessibility).

During or after conferences, be sure to bug him about those last points: getting those right is a prerequisite in effectively getting daily work done.

---

Title: If you thought you could do multi-threading, then play “The Deadlock Empire” games

Abstract:

This is about a Delphi specific web game focussing on the concurrency issues in multi-threading environments. Each round you play the machine that executes the example code and tries to let it crash in a multi-threading way. Be inventive, think like a bad guy and let the two threads in the sample code deadlock, end up in the same critical section or fail in a debugging assertion.

During the workshop we will play each round interactively: all attendees play the round followed by a short discussion. This is about collective learning, so the speaker will probably learn just as much as attendees do: teaching is learning and learning is teaching.

During the conference you will also get a link to the orginal C# version from which Jeroen derived the Delphi version.

Difficulty level: 1 (intermediate)

---

Title: Hidden Gems of Delphi Language: Operator Overloading and Class/Record helpers

Abstract:

Originally added to the Delphi .NET compiler, both operator overloading and class/record helpers found themselves into the native Delphi compiler as well.

With Delphi XE3, the helpers were extended to support simple types (like integers and strings) allowing even more flexibility.

This session will show you some examples (from my own, from other people on the internet and from the Delphi RTL, VCL and FireMonkey) demonstrating their power, but also making you aware of limitations.

The Delphi helpers allow you to extend existing Delphi libraries at any place in the inheritance hierarchy without changing the libraries. The operators will allow you to redefine operators for record types allowing for some nifty code.

Some operator overloading examples covered: the underdocumented table of operators you can overload and what each operator does, various kinds of nullable types (integers, floating points), fractions

Some helper examples: adding `for` ... `in` support to various portions of the Delphi run-time libraries, getting the source file name of an `EAssertionFailed`, better casting and a more safe `FreeAndNil` method, adding operators to existing classes

Difficulty level: 2 (intermediate)

---

required:


    Title (for every talk)
    Abstract (for every talk)
    Difficulty level (for every talk)
        Difficulty level is a scale from 1 to 3 with the following mean: introduction, intermediate, advanced
    Speaker’s photo (at least 500x500px)
    Speaker’s profile

---

Notes

https://stackexchange.com/users/14295

https://deadlockempire.github.io/