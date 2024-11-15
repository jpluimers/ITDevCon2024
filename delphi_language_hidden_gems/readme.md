![header](../images/ITDevCon2024.Theme.00.Title-Slide.header.png)

# [Hidden Gems of Delphi Language: Operator Overloading and Class/Record helpers](https://www.itdevcon.it/roma/en/sessions/operator_overloading_in_real_world/)

Abstract:

Originally added to the Delphi .NET compiler, both operator overloading and class/record helpers found themselves into the native Delphi compiler as well.

With Delphi XE3, the helpers were extended to support simple types (like integers and strings) allowing even more flexibility.

This session will show you some examples (from my own, from other people on the internet and from the Delphi RTL, VCL and FireMonkey) demonstrating their power, but also making you aware of limitations.

The Delphi helpers allow you to extend existing Delphi libraries at any place in the inheritance hierarchy without changing the libraries. The operators will allow you to redefine operators for record types allowing for some nifty code.

Some operator overloading examples covered: the underdocumented table of operators you can overload and what each operator does, various kinds of nullable types (integers, floating points), fractions

Some helper examples: adding `for` ... `in` support to various portions of the Delphi run-time libraries, getting the source file name of an `EAssertionFailed`, better casting and a more safe `FreeAndNil` method, adding operators to existing classes

Difficulty level: 2 (intermediate)

The content will be at [delphi_language_hidden_gems.md](./delphi_language_hidden_gems.md)

![footer](../images/ITDevCon2024.Theme.00.Title-Slide.footer.png)