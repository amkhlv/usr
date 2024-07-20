
import { ALink } from './Utils';

function MathWriteups() {
  function sitelink(u) {
    window.location.href = u
  }
  return (
    <div className='mathwriteups'>
      <h3>Some of my talks</h3>

      <p></p>

      <p>
        <ALink href="slides/ift_2012-03-26/index.html">Talk in IFT, March 2012</ALink>
        {'\u00A0'}
        about deformations of BRST structure
      </p>

      <p>
        <ALink href="slides/Natal_2012/index.html">Talk in Natal</ALink>, May 2012
        about the massless SUGRA vertex in AdS
      </p>

      <p>
        Talk in Passa Quatro, August 2012:
        <ALink href="slides/PassaQuatro-slides/index.html">slides</ALink>,
        <ALink href="slides/PassaQuatro-singlepage/talk_PassaQuatro.html">single page</ALink>,
        on the use of Lie algebra cohomology in Type IIB SUGRA
      </p>


      <p>
        <ALink href="slides/talk_ITEP_2012/index.html">Talk at ITEP about linear dilaton</ALink>, December 2012
      </p>

      <p>
        <ALink href="slides/talk_Brasilia/index.html">Talk in Brasília about the integrated vertex</ALink>, November 2013
      </p>

      <p>
        <ALink href="slides/talk_Perimeter/index.html">Talk at the workshop on perturbative string theory, Perimeter Institute</ALink>, April 2015
      </p>

      <p>
        <ALink href="slides/talk_IFT-math_2016/index.html">BV formalism and string amplitudes</ALink>, mathematical physics workshop, SAIFR, June 2016
      </p>

      <p>
        <ALink href="slides/2019_workshop_quantum-symmetries/index.html">Symmetries, renormgroup, deformations, AdS/CFT</ALink>,
        &nbsp;
        <ALink href="https://www.ictp-saifr.org/workshop-on-quantum-symmetries/">workshop on quantum symmeties</ALink>, SAIFR, October 2019
      </p>

      <p>
        <ALink href="slides/talk_ESI_2021/index.html">BV formalism for string worldsheet</ALink>,
        &nbsp;
        Geometry for Higher Spin Gravity: Conformal Structures, PDEs, and Q-manifolds
        <ALink href="https://www.esi.ac.at/events/e394/">(ESI, September 2021)</ALink>
      </p>

      <p>
        <ALink href="slides/talk_SFTCloud_2021/index.html">Equivariant BV formalism for string worldsheet</ALink>
        &nbsp;
        SFT@Cloud <a href="https://indico.cern.ch/event/1042834/">(Cloud, September 2021)</a>
      </p>

      <p>
        <ALink href="slides/noncovariance/index.html">Quadratic algebras and symmetry transformations of vertex operators</ALink>
        &emsp;
        <a href="https://www.ictp-saifr.org/symposium12/">ICTP-SAIFR 12th Anniversary Symposium: Physics for South America</a>
      </p>

      <h3>Writeups</h3>

      <p></p>

      <p>
        <ALink href="/math/bv/index.html">BV formalism</ALink>
      </p>

      <p>
        <ALink href="/math/pure-spinor-formalism/index.html">Pure spinor formulas</ALink>
      </p>

      <p>
        <ALink href="/math/deformations-of-AdS/finite-dimensional-vertex/index.html">Finite dimensional vertex</ALink>
      </p>

      <p>
        <ALink href="/math/projector/index.html">BV description of AdS superstring</ALink>
      </p>

      <p>
        <ALink href="/math/Renormgroup/index.html">Deformations, renormgroup, symmetries, AdS/CFT</ALink>
      </p>

      <p>
        <ALink href="/math/DgAndBV/index.html">DGLA Dg and BV formalism</ALink> (also <ALink href="slides/talk_SFTCloud_2021/index.html">talk slides</ALink>)

      </p>

      <p>
        <ALink href="/math/vector-fields/index.html">Normal form of nilpotent vector field near the tip of the pure spinor cone</ALink>
      </p>

      <p>
        <ALink href="math/text_brackets/index.html">Derived brackets in bosonic string worldsheet sigma-model</ALink>
      </p>

      <p>
        <ALink href="/math/InsertionUsingBV/index.html">Insertion of vertex operators using BV</ALink>
      </p>

      <p>
        <ALink href="/math/worldsheet-currents/index.html">Symmetry transformations of worldsheet currents</ALink>
      </p>



    </div>


  )

}

export default MathWriteups;
