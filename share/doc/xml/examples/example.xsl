<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns="http://www.w3.org/1999/xhtml">
 
  <xsl:template match="/catalog">
    <html>
      <body>
        <table border="1">
          <tr>
            <td><b>author</b></td>
            <td><b>title</b></td>
            <td><b>price</b></td>
          </tr>
          <xsl:for-each select="book">
            <tr>
              <td><xsl:value-of select="author"/></td>
              <td><xsl:value-of select="title"/></td>
              <td><xsl:value-of select="price"/></td>
            </tr>
          </xsl:for-each>
        </table>
      </body>
    </html>
  </xsl:template>
 
</xsl:stylesheet>
