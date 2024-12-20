#!/usr/bin/env -S nu --stdin

def main [] {
    let svg_filepath = (xsel -b | sed -e's!^file://!!')
    let svg = (cat $svg_filepath | from xml)
    let rootattrs = ($svg | get "attributes")
    let width = ($rootattrs | get "width" | sed -e's/[a-z]*//g' | into float)
    let rootattrs_new = if "height" in ($rootattrs | columns) {
        let height = ($rootattrs | get "height" | sed -e's/[a-z]*//g' | into float)
        $rootattrs | 
        update width $"(1.25 * $width)" | 
        update height $"(1.25 * $height)" | 
        update viewBox $"0,0,(50 * $width),(50 * $height)" 
    } else {
        $rootattrs | 
        update width $"(1.25 * $width)" | 
        update viewBox $"0,0,(50 * $width),(50 * $width)" 
        } 
            

    let svg_new = $svg | update "attributes" $rootattrs_new
    $svg_new | to xml | save --force "/tmp/svg_resized.svg"
    echo "/tmp/svg_resized.svg" | xsel -ib
}



