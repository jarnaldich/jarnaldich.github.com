---
title: Porting the Unix Philosophy to Windows
date: 2017-06-06T00:00:00
tags: powershell, unix, windows
---


Since 2016, it is safe to say that windows has
a [pretty decent shell](https://en.wikipedia.org/wiki/PowerShell). Actually,
it's had it from some time now, but on August 2016 went open-source and
cross-platform. Although it is still common to find old-style `.bat` files
lingering around in many organizations, it looks clear that PowerShell is
getting out of its initial sysadmin niche towards becoming the new de-facto
standard shell for Windows (hey, even
for
[malware...](https://www.symantec.com/content/dam/symantec/docs/security-center/white-papers/increased-use-of-powershell-in-attacks-16-en.pdf)).
And no, I do not even think WSH deserves a mention.

Arguably, the most profound change use of PowerShell is not having a more
powerful (ehem!) shell at windows, but enabling the kind of scripting Unix has
excelled at before.

## The Unix Philosophy
The [Unix Philosophy](https://en.wikipedia.org/wiki/Unix_philosophy) is often
epitomized in one sentence:

> Do One Thing and Do It Well

The implications of that for shell scripting are translated into the fact that
Unix has lots of small executables devoted to one task, and it is the shell's
responsibility to enable composing these bits of functionality into more complex
ones, the most prominent tool for that being the pipe `|` operator, which feeds
the output of a program into the input of the next.

Powershell has a more ore less generalized version of this, where the pieces are
called `CmdLets`, and what is sent down the pipe is a stream of _CLR objects_,
not just a stream of bytes. Before PowerShell, it was impossible to do this in
Windows to the extent that it was in Unix.

The idea is simple, but the skill is difficult to master. Shell scripting _is_
programming, but the abstractions provided by the shell are different from the
ones you would find in a fully-fledged programming language.

In this blog post I would like to present a particular example of what this
change means.

# The Task
A quick and dirty way to monitor the progress of a batch process is probing the
number of output files in a directory at regular intervals and maybe save that
to a file eg. for plotting, statistics, etc...

It is quite probable that a developer would come up with a solution very much
like the function below:

```powershell
function Sample-Count-Files {
    [CmdletBinding()]

    param(
        # The file pattern to count
        [Parameter(Mandatory=$true, 
                   Position=0)]
        [string]$pattern,

        # Name of the log file
        [Parameter(Mandatory=$true, 
            Position=1)]
        [string]$logfile,

        # Seconds interval between samples
        [Parameter(Mandatory=$true, 
            Position=2)]
        [int]$seconds)

    While($true) {
        sleep $seconds;
        $cnt = (dir $pattern).Count;
        $d = Get-Date;
        $d.ToString("yyyy-MM-dd HH:mm:ss") + "`t$cnt" | Out-File $logfile -encoding ascii -Append -ob 0
    }
}

```

That is, an infinte loop which waits for a number of seconds before counting the
output files and outputting a line with the date and result. Executing the above
code will block the console, so to monitor progress you can always wrap it into
a `PSJob` or open a new console and then just tail the file:

```powershell
gc -tail 10 -Wait samples.tsv
```

I suspect most developers that have not dealt with Unix scripting would come up
with something along the lines of this. Conversely, my bet is any experienced
Unix scripter would frown upon it, feeling the script is trying to do _too
much_. In particular, it is in charge of:

* **The When**: The counting is done every _n_ seconds. 
* **The What**: The counting itself.
* **The Output**: We are saving into a text file and redirecting to screen.

These three pieces of functionality are coupled in our function, and they would'nt need to be so. For example, separating the _when_ from the _what_ would allow us to fire  _any action_  every _n_ seconds. 

This is indeed not difficult in PowerShell, see:

```powershell
function Tick {
    [CmdletBinding()]
    Param([Parameter(Mandatory=$true, Position=0)][int]$Seconds)
    Process {
        while($true) { 
            Start-Sleep -Seconds $Seconds
            Get-Date
        }    
    }
}
```

This `CmdLet` waits for a number of seconds before sending a `Date` object downstream to do whatever we please with it, and loops (mind it is inside the `Process` section). It is like a pulse generating objects at regular intervals, so now we can reuse the _when_ in different contexts:

```powershell
Tick 10 | % { $_ }
```

We can write a similar function for the _what_. In a real scenario we probably would just write a one-liner, because our code is so simple, but in this post we will go the full way. As a bonus, instead of working with strings, we demonstrate how to pass custom objects down the pipeline. Here, we create an object with two members: the time and the file count.

```powershell

function Count {
    [CmdletBinding()]
    Param(
    	[Parameter(Mandatory=$true, Position=0)]
	    [string]
	    $Pattern,

        [Parameter(Mandatory=$true, Position=1, ValueFromPipeline=$true)]
	    $Time
    )

    Process {
        [PSCustomObject]@{ 
           Time=$Time; 
           Files= $(dir $Pattern).Count 
        }
    }
}

```

For the output, we could write our own function again, but it turns out there
already is a Powershell function that formats objects into CSV files,
conveniently named `Export-Csv`. Putting it all together:

```powershell
Tick 1 -ob 0 | Count *.out | Export-Csv times.csv
```

Which is clean, easy to understand and easy to reuse, just like good _Unix_
scripting is supposed to. By the way, if you are wondering where the `-ob 0` or
`-ObjectBuffer 0` came from (we did not explicitly add it to our script), that
is known as
a
[common parameter](https://msdn.microsoft.com/en-us/powershell/reference/5.1/microsoft.powershell.core/about/about_commonparameters).
For efficiency reasons, Powershell can wait until a bunch of objects are
accumulated into a buffer before sending them downstream. Obviously that is not
what we want here, so we set the buffer size to 0.

## Conclusion
It is often a good idea, when approaching shell scripting, to take a step back
and think whether we are trying to accomplish too much at once, and which pieces
of functionality we would like to reuse in the future. That is not really
different from the software architecture best practices applied when programming
in the large, but the abstractions (programming whith pipes and streams) are.
Now Windows programmers can apply the same principles at play in Unix for
decades.

## See also
- The sampling approach is probably too naive. For more robust approaches one
  should probably take a look into the `Timer` and `FileWatcher` events.
- This
  [Gist](https://gist.github.com/jarnaldich/67296c892cde9f9c5bbe9d7ccac97ee9)
  has the code for this article.
