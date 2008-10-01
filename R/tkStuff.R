tkpager <- function (file, header, title, delete.file) {
    # from tcltk package, but with white bg
    for (i in seq(along = file)) {
        zfile <- file[[i]]
        tt <- tktoplevel()
        tkwm.title(tt, if (length(title)) 
            title[(i - 1)%%length(title) + 1]
        else "")
        txt <- tktext(tt, bg = "white")
        scr <- tkscrollbar(tt, repeatinterval = 5, command = function(...) tkyview(txt, 
            ...))
        tkconfigure(txt, yscrollcommand = function(...) tkset(scr, 
            ...))
        tkpack(txt, side = "left", fill = "both", expand = TRUE)
        tkpack(scr, side = "right", fill = "y")
        chn <- tcl("open", zfile)
        tkinsert(txt, "end", header[[i]])
        tkinsert(txt, "end", gsub("_\b", "", tclvalue(tcl("read", 
            chn))))
        tcl("close", chn)
        tkconfigure(txt, state = "disabled")
        tkmark.set(txt, "insert", "0.0")
        tkfocus(txt)
        if (delete.file) 
            tcl("file", "delete", zfile)
    }
}
