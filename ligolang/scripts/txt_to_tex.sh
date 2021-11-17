
input=$1
output=${input%.*}.tex

echo "\documentclass[10pt,a4paper]{article}" > $output 
echo "\usepackage[utf8]{inputenc}"  >> $output
echo "\usepackage[english]{babel}" >> $output
echo "\usepackage[fleqn]{amsmath}" >> $output
echo "\usepackage{amsfonts}" >> $output
echo "\usepackage{amssymb}" >> $output
echo "\usepackage{graphicx}" >> $output
echo "\usepackage{listings}" >> $output
echo "\usepackage[left=2cm,right=2cm,top=2cm,bottom=2cm]{geometry}" >> $output
echo "\title{Ligo Formal Description}" >> $output
echo "\lstset{language=caml}" >> $output
echo "\author{}" >> $output
echo "\date{}" >> $output
echo "\begin{document}" >> $output
echo "\maketitle" >> $output



sed \
    -e '/[a-z ]*\(([a-zA-Z]\(,[a-zA-Z]\)\{0,1\})\|=\|([a-zA-Z]\{1,2\}) =\)[ ]*$/{
        i\\\begin{align*}
        s/ \([^ ]\)/\\\ \1/g
        b skip
        :l
        i\\\\\\
        :skip
        n
        s/^  \(| .*\)   *(\* \(.*\) \*)/  \&\1\\tag{\2}/
        t l
        i\\\end{align*}
        d
    }' ./ligo_formal_description.txt | \
sed \
    -e "/^[^\\]/ {
        s/\(^\|[^g]\){/\1\\\{/g 
        s/}\([^$]\|  *$\)/\\\}\1/g
    }" \
    -e "s/^\([^|]*\) ==> \(.*[^ ]\)\( *\)(\*\(.*\)\*)/\\\begin{equation}\\\frac{\1}{\2} \\\tag{\4}\\\end{equation}/" \
    -e "s/^\([^|]*\)->\(.*[^ ]\) *(\*\(.*\)\*)/\\\begin{equation}\1->\2 \\\tag{\3}\\\end{equation}/" \
    -e "s/^\([^|]*\) ==== \(.*\)/\\\begin{equation*}\1 \\\iff \2 \\\end{equation*}/" \
    -e "s/(\*\*\(.*\)\*\*)/\\\section\*{\1}/g" \
    -e "s/^(\*\(.*\)\*)/\\\subsection\*{\1}/g" \
    -e "/^\\\begin/ s/ \([^ ]\)/\\\ \1/g" \
    -e "/^  \&/ s/ \([^ ]\)/\\\ \1/g" \
    -e "s/λ/\\\lambda /g" \
    -e "s/-->/\\\mapsto /g" \
    -e "s/->/\\\rightarrow /g" \
    -e "s/\([a-zA-Z]*\) ==> \([a-zA-Z]*\)/\\\frac{\1}{\2}/g" \
    >> $output

echo "\end{document}" >> $output
pdflatex $output
