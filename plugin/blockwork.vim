"
" BlockWork - Adjust list numbering and other work with (visual) block selections
"             Function to work with selected blocks --
"             includes vmap assignments for incrementing, decrementing,
"             and resequencing integers in a selected block.
"
"  Maintainer: John R. Aldridge, Jr.
"     Created: 2002-12-14
" Last Change: 2002-12-14
"     Version: 0.0
" 
"------------------------------------------------------------------------
" Purpose:
" Simplifies editing numbered lists or other sequences of numbers. 
" Includes function to apply most any command to a selected
" 'visual block' of text.
"------------------------------------------------------------------------
" Justification:
" Vim includes CTRL-A and CTRL-X to add and subract from integers
" under the cursor, however neither of these works on selections.
" There are search and replace techniques to handle multiple 
" changes on a range of lines (e.g. s/-\?\d\+/\=submatch(0)+1/g').
" However, this won't work on a 'visual block' selection.
"------------------------------------------------------------------------
" Example:
" The following list had an item deleted
" and you want to decrement all the numbers by 1.
"      2) show 35
"      3) show 32
"      ...
"    134) show 12
" Using this plugin, you would position the cursor on the 2, 
" select the column of numbes and decrement -- i.e. type:
"    <C-V>133jb<C-X> 
" (where <C-V> and <C-X> are CTRL-V and CTRL-X respectively)
"------------------------------------------------------------------------
" Techniques:
" Most of the work is performed by the general purpose BlockWork 
" function.  In this plugin, vmaps are provided to increment, decrement,
" and resequence integers in selected blocks of text.  However,
" BlockWork should also be able to handle other commands as well.
" The argument that BlockWork takes is any string that can be applied 
" to the :execute command.  That execute command will be performed
" on only the selected text (after being copied to a temporary buffer).
"------------------------------------------------------------------------
" Issues:
" 1) The window in which the selection is made in must be large enough
"    ('lines', 'columns') to allow opening one additional window.
"    Otherwise, an error will occur.  This temporary window should not
"    become visible, but some shifting of splits may occur.
" 2) Reselection marks two additional undo points.  If you make three
"    increments, it will take five undo's (u) to return to the start.
" 3) Additional code adjusts the width of the selection if the altered
"    text also changes in size.  However, this works inconsistently.
"    With or without this adjustment, the selection may need some
"    manual adjustment.
"------------------------------------------------------------------------
" To test this file:
"    1. open it in Vim         
"    2. source it             |  :so %<Enter>
"    3. go to the sample text |  /Sample<Enter>
"    4. select some text      |  jf(<C-V>j2f)
"    5. do some incrementing  |  <C-A>
"    6. and decrementing      |  <C-X>
"    7. try the resequence    |  4j<M-S-A>
"------------------------------------------------------------------------
" To use this file as a plugin, place it in the plugin directory.
" (:help plugin)
"------------------------------------------------------------------------

" VMap: Function to increment/decrement numbers in a selected block
vmap <C-A> :call BlockWork('%s/-\?\d\+/\=submatch(0)+1/g')<CR>
vmap <C-X> :call BlockWork('%s/-\?\d\+/\=submatch(0)-1/g')<CR>

" VMap: Function to resequence numbers by line in a selected block
vmap <M-S-A> :call BlockWork('%call ReSequence(".",1)')<CR>
vmap <M-S-X> :call BlockWork('%call ReSequence(".",-1)')<CR>

"------------------------------------------------------------------------
" Resequence numbers by line in a selected block.
" The left-most numbers are put in sequence form line to line.
" Additional numbers on each line are adjusted by the same amount
" as the left-most number of the line.
"------------------------------------------------------------------------
" a:Initial, if provided, is the number that will start the sequence.
"            If not provided, the initial value is the first number.
" a:Increment (required) is the step each line changes by.
"------------------------------------------------------------------------
function! ReSequence(Initial, Increment) range

  " Go to the first number line and select an initial value

    " Find the first numbered line
    let l:ln = a:firstline
    while getline(l:ln) !~ '\d' && l:ln <= a:lastline
        let l:ln = l:ln + 1
    endwhile

    " Select an initial value
    if a:Initial =~ '^\d\+$'
        let l:cnt = a:Initial
    else
        " The first integer
        let l:cnt = substitute(getline(l:ln), '^\D*\(\d\+\).*', '\1', '')
    endif

  " For each line set the numbers
    while l:ln <= a:lastline
        if getline(l:ln) =~ '\d'
            let l:delta = l:cnt - substitute(getline(l:ln), '^\D*\(\d\+\).*', '\1', '')
            execute l:ln . 's/\d\+/\=submatch(0) + ' . l:delta . '/g'
            let l:cnt = l:cnt + a:Increment
        endif
        let l:ln = l:ln + 1
    endwhile

endfunction

"------------------------------------------------------------------------
" Once in an empty buffer,
" paste the text, execute the command,
" and yank the text back again.
function! s:BlockWorkInBuffer(ExecuteStr)

  " Put the text in the buffer

    " Capture block type for future copy back (must be in new buffer)
    let l:RegType = getregtype('"')

    " Paste the captured block
    norm p

  " Work on the text
    execute a:ExecuteStr

  " Collect the altered text

    " If selection was a visual block
    if l:RegType =~ "^\<C-V>"

        " Retrieve visual-block work
        silent! execute "norm gg\<C-V>G$y"

    else

        " Retrieve non-visual-block work
        " silent! norm ggyG
        silent! norm ggvG$y

        " Set register type (by appending nothing with reg type)
        call setreg('"', '', 'a' . l:RegType)

    endif

endfunction


"------------------------------------------------------------------------
" Create the empty buffer for the work,
" do the work, and then exit the buffer.
"------------------------------------------------------------------------
" This technique requires a minimum amount of space to create a
" temporary window (can't create a hidden one and work there),
" and it may result in some changes to splits.
"------------------------------------------------------------------------
function! s:BlockWorkBuffer(ExecuteStr)

    " Create a buffer marker to avoid working in the wrong buffer
    let b:BlockWorkBufferStart = 1

    " Calm down the window changes
    let b:TempBufferEa = &equalalways
    set noea

    try

     " Open the temp buffer (if there's enough window space)
        execute 'new ++ff=' . &ff . ' enc=' . &enc . ' +set\ bufhidden=wipe'
        set buftype=nofile noswapfile

        if exists('b:BlockWorkBufferStart')
            throw "BlockWorkBuffer: failed to create empty buffer"
        endif

     " Work on the text
        call s:BlockWorkInBuffer(a:ExecuteStr)

     " Close the temp buffer
        " quit
        bdelete

    finally
        let &equalalways = b:TempBufferEa
        unlet b:TempBufferEa
        unlet b:BlockWorkBufferStart
    endtry

endfunction


"------------------------------------------------------------------------
" Make a selection of the type saved at the corners saved.
" Used to recover the selection that is lost when
" pasting into selection (not needed for pasting into a visual block).
"------------------------------------------------------------------------
" This sets the marks by moving the cursor,
" and therefore creates two undo points.
" Would be better if the marks could be set directly.
"------------------------------------------------------------------------
function! SetSelectionMarks(Line1, Col1, Line2, Col2, RegType)

    let l:Line1 = a:Line1
    let l:Col1 = a:Col1
    let l:Line2 = a:Line2
    let l:Col2 = a:Col2

    " If a width is included,
    " determine if the selection width needs to be altered
 " UNTESTED (remove the -1 &&)
    if  a:RegType =~ '\d'
        " Extract the reg width
        let l:rwid = substitute(a:RegType, '\D', '', '')
        " How many assumptions are we making here???
        if l:Col2 >= l:Col1
            let l:Col2 = l:Col1 + l:rwid
        else
            let l:Col1 = l:Col2 + l:rwid
        endif
    endif

    " Its up to the caller to save and restore the current position

    " Go To the start of the selection
    execute "norm " . l:Line1 . "G" . l:Col1 . "|"

    if a:RegType == 'v' || a:RegType == 'c'
        " Character wise
        norm v
    elseif a:RegType == 'V' || a:RegType == 'l'
        " Line wise
        norm V
    elseif a:RegType == '-1'
        " We have to know what type of selection to make
        throw "SetSelectionMarks: unknown block selection type"
    else
        " Assume visual block
        execute "norm \<C-V>"
    endif

    execute "norm " . l:Line2 . "G" . l:Col2 . "|"

endfunction


"------------------------------------------------------------------------
" Setup the yank the selection,
" do the work, and replace the selected text.
"------------------------------------------------------------------------
function! s:BlockWorkExecute(ExecuteStr)

  " Grab the text

    " Because we were called from a vmap,
    " we can assume that the visual marks are set.
    " Re-select the block and yank
    norm gvy

    " Capture selection marks -- which may change after paste
    let l:Line0 = line("'<")
    let l:Col0 = virtcol("'<")
    let l:Line1 = line("'>")
    let l:Col1 = virtcol("'>")


  " Work on the text
    call s:BlockWorkBuffer(a:ExecuteStr)

  " Put the changed text back

    " Capture block type for future reselection with above mark variables
    let l:RegType = getregtype('"')

    " Reselect the block and paste
    norm gvP

    " Reselect the block one more time (for repeats)
    " norm gv
    call SetSelectionMarks(l:Line0, l:Col0, l:Line1, l:Col1, l:RegType)

endfunction

"------------------------------------------------------------------------
" Accepts a task to be performed (as an execute command),
" and performs that task on a selected block (most recent) of text.
"------------------------------------------------------------------------
function! BlockWork(ExecuteStr) range

    " Save contents of the register that will be used for capturing the text
    let l:sav_regt = getregtype('"')
    let l:sav_reg = getreg('"')

    try
        call s:BlockWorkExecute(a:ExecuteStr)
    catch /./
        echoerr v:exception
        return 0
    finally
        " Restore register
        call setreg('"', l:sav_reg, l:sav_regt)
    endtry

    return 1

endfunction


" Sample text
" (0=zero)  (1=one)     (2=two)   (3=three)
" (4=four)  (5=five)    (8=eight) (9=nine)
" 3  4    " 3  4
" 5  6    " 5  6
" 9  10   " 9  10
" 10  11  " 10  11
"23456789012345678
"2          3456789
"23         34567890
"234        345678901
"2345       3456789012
"23456      34567890123
"234567     345678901234
"2345678    3456789012345
"23456789   34567890123456
"234567890  345678901234567
"2345678901 3456789012345678

"   2   4   6   8   0   2   4   6   8

" vi: set tabstop=4 sw=4:
" vim: set expandtab:
