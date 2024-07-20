import { ALink } from './Utils';

function Computing() {
  return(
      <div className='computing'>
        <h1>Computer tips and tricks</h1>
        <p></p>

          <p>
          <ALink href="https://amkhlv.github.io/bystrotex-manual/">BystroTeX</ALink> --- my method to insert LaTeX formulas into HTML
          </p>
          
          <p>
          <a href="https://github.com/amkhlv/mathpump3/blob/master/docs/inkscape.md">Notes on how to use Inkscape</a> (not entirely trivial!)
          </p>

          <p>
          <ALink href="/scribbles/keyboard-layout/keyboard-layout.html">Notes on keyboard layouts</ALink>
          </p>

      </div>
    
  )
} 

export default Computing;
