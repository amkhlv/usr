# Using parbox instead of minipage

Instead of `minipage` it is better to use `parbox`:

    \noindent
    \parbox[b][4em][t]{0.33\textwidth}{Some \\ text} 
    \parbox[c][4em][s]{0.33\textwidth}{Some \vfill text} 
    \parbox[t][4em][c]{0.33\textwidth}{Some \\ text} 



The format is:

    \parbox[pos1][height][pos2]{content}



Parameters:

> &#x3d;item\* pos1 
>
> can be: `c,t,b` --- this is the position of the parbox relative to the baseline of the surrounding text. 
>
> &#x3d;item\* height
>
> the height of the parbox
>
> &#x3d;item\* pos2
>
> can be: `c,t,b,s` --- the position of the content inside the parbox, where `s` stands for \`\`spread''.
