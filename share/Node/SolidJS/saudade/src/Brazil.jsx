import { ALink } from './Utils';

function Brazil() {
  return (
    <div className='br'>
      <h1>Some info about Brazil</h1>

      <p></p>

      <p>
        <ALink href="/scribbles/bureaucracy/index.html">Bureaucracy</ALink>
      </p>

      <p>
        <ALink href="/scribbles/invalid-certs.html">Invalid certificates on Brazil Government sites</ALink>
      </p>

      <p>
        <ALink href="/scribbles/directions-to-IFT.html">Directions to IFT</ALink>
      </p>

      <p>
        <ALink href="/scribbles/hotels/index.html">Hotels near IFT</ALink>
      </p>

      <p>
        <ALink href="/scribbles/telephone.html">How to use telephone</ALink>
      </p>

      <p>
        <a href="https://play.google.com/store/apps/details?id=org.mistergroup.shouldianswer&hl=pt&gl=US">
          <img src="/images/devo-atender.png"></img> ``Should I answer?'' --- an excellent Android app to block unwanted phone calls
        </a>
      </p>

    </div>
  )
}

export default Brazil;
