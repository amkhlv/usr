export function ALink(props) {
  return(
    <a href={props.href} onClick = {(e) => window.location.href = props.href}>{props.children}</a>  
  )
}

