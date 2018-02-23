# Delphi High Performance
This is the code repository for [Delphi High Performance](https://www.packtpub.com/application-development/delphi-high-performance?utm_source=github&utm_medium=repository&utm_campaign=9781788625456), published by [Packt](https://www.packtpub.com/?utm_source=github). It contains all the supporting project files necessary to work through the book from start to finish.
## About the Book
Delphi is a cross-platform Integrated Development Environment (IDE) that supports rapid application development for Microsoft Windows, Apple Mac OS X, Google Android, iOS, and now Linux with RAD Studio 10.2. This book will be your guide to build efficient high Performance applications with Delphi.

The book begins by explaining you how to find performance bottlenecks and apply the correct algorithm to fix them. It will teach you how to improve your algorithms before taking you through Parallel programming and explore the various tools to build highly concurrent applications. You will then delve into improving the performance of your code and master the cross platform RTL improvements. Finally, this book will take you through memory management with Delphi and help you leverage the several external libraries to write better performing programs.

By the end of the book, you will be adept with the knowledge to create High Performance Applications with Delphi.

## Instructions and Navigation
All of the code is organized into folders. Each folder starts with a number followed by the application name. For example, Chapter02.

There is no code in chapter 9.

The code will look like the following:
```
function IsPresentInList(strings: TStrings; const value: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to strings.Count - 1 do 
    if SameText(strings[i], value) then 
      Exit(True);
end;
```

Although you can read this book in bed or on the beach, you will need a computer and Delphi to play with the code examples. The code was written in Delphi 10.2 Tokyo, but it should also work without a problem in the older versions. I did use some modern features in demos—and dedicated a chapter to Parallel Programming Library that was introduced in Delphi XE7—so anything older than that is hit and miss.

This book does not refer to any functionality specific to the Enterprise edition. You'll be able to test all the code with the entry-level professional edition.

## Related Products
* [Expert Delphi](https://www.packtpub.com/application-development/expert-delphi?utm_source=github&utm_medium=repository&utm_campaign=9781786460165)

* [Delphi Cookbook](https://www.packtpub.com/application-development/delphi-cookbook?utm_source=github&utm_medium=repository&utm_campaign=9781783559589)

* [Delphi Cookbook - Second Edition](https://www.packtpub.com/application-development/delphi-cookbook-second-edition?utm_source=github&utm_medium=repository&utm_campaign=9781785287428)

### Suggestions and Feedback
[Click here](https://docs.google.com/forms/d/e/1FAIpQLSe5qwunkGf6PUvzPirPDtuy1Du5Rlzew23UBp2S-P3wB-GcwQ/viewform) if you have any feedback or suggestions.
