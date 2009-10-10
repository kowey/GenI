<!-- xmg2geni.xsl

     Converts a TAG grammar from XMG's XML format into GenI's text
     format, in replacement of `geniconvert -f xmg -t geni'.

     Usage:
       xsltproc -o grammar.geni xmg2geni.xsl grammar.xmg

     Copyright (c) 2007 INRIA
     Author Sylvain Schmitz <Sylvain.Schmitz@loria.fr>

     Permission is hereby granted, free of charge, to any person
     obtaining a copy of this software and associated documentation
     files (the "Software"), to deal in the Software without
     restriction, including without limitation the rights to use,
     copy, modify, merge, publish, distribute, sublicense, and/or sell
     copies of the Software, and to permit persons to whom the
     Software is furnished to do so, subject to the following
     conditions:

     The above copyright notice and this permission notice shall be
     included in all copies or substantial portions of the Software.

     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
     EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
     OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
     NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
     HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
     WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
     FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
     OTHER DEALINGS IN THE SOFTWARE.
  -->
<xsl:stylesheet version="1.0" 
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text" encoding="utf-8"/>
<xsl:strip-space elements="*"/>
<xsl:variable name="depth" select="count(//node)"/>

<xsl:template match="entry">
  <!-- family and tree name -->
  <xsl:text>
% ------------------------- </xsl:text>
  <xsl:value-of select="@name"/>
  <xsl:text>
</xsl:text>
  <xsl:value-of select="family"/>
  <xsl:text>:</xsl:text>
  <xsl:value-of select="@name"/>
  
  <!-- interface -->
  <xsl:text> ( ! </xsl:text>
  <xsl:apply-templates select="interface"/>
  <xsl:text>) </xsl:text>

  <!-- initial or auxiliary tree -->
  <xsl:choose>
    <xsl:when test="tree//node[@type='foot']">
      <xsl:text>auxiliary
</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>initial
</xsl:text>
    </xsl:otherwise>
  </xsl:choose>

  <xsl:apply-templates select="tree"/>
  <xsl:apply-templates select="semantics"/>
  <xsl:apply-templates select="trace"/>
  <xsl:text>
</xsl:text>
</xsl:template>

<!--========================================================= tree -->

<xsl:template match="tree">
  <xsl:for-each select="node">
    <xsl:call-template name="donode">
      <xsl:with-param name="indent" select="''"/>
      <xsl:with-param name="count" select="0"/>
    </xsl:call-template>
  </xsl:for-each>
</xsl:template>

<xsl:template name="donode">
  <xsl:param name="indent"/>
  <xsl:param name="count"/>

  <!-- node name -->
  <xsl:value-of select="$indent"/>
  <xsl:choose>
    <xsl:when test="@name">
      <xsl:value-of select="@name"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>n</xsl:text>
      <xsl:value-of select="$count"/>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text> </xsl:text>

  <!-- node type -->
  <xsl:choose>
    <xsl:when test="@type!='std' and @type!='nadj'">
      <xsl:text>type:</xsl:text>
      <xsl:value-of select="@type"/>
      <xsl:text> </xsl:text>
    </xsl:when>
    <xsl:when test="@type='nadj'">
      <xsl:text>aconstr:noadj </xsl:text>
    </xsl:when>
  </xsl:choose>

  <!-- coanchors -->
  <xsl:if test="@type = 'lex'">
    <xsl:text>"</xsl:text>
    <xsl:value-of select="narg/fs/f/sym/@value"/>
    <xsl:text>" </xsl:text>
  </xsl:if>

  <!-- features -->
  <xsl:for-each select="narg/fs">
    <xsl:text>[</xsl:text>
    <xsl:apply-templates select="f[@name='cat']|f[@name='top']/fs/f|f[@name!='cat' and @name!='top' and @name!='bot']">
      <xsl:sort select="@name"/>
    </xsl:apply-templates>
    <xsl:text>]</xsl:text>
    <xsl:text>!</xsl:text>
    <xsl:text>[</xsl:text>
    <xsl:apply-templates select="f[@name='cat']|f[@name='bot']/fs/f|f[@name!='cat' and @name!='top' and @name!='bot']">
      <xsl:sort select="@name"/>
    </xsl:apply-templates>
    <xsl:text>]</xsl:text>
  </xsl:for-each>
  
  <!-- children -->
  <xsl:if test="node">
    <xsl:text>{
</xsl:text>
    <xsl:for-each select="node">
      <xsl:call-template name="donode">
        <xsl:with-param name="indent" select="concat(' ',$indent)"/>
        <xsl:with-param name="count"
                        select="$count + 1 + count(preceding-sibling::node/descendant-or-self::node)"/>
      </xsl:call-template>
    </xsl:for-each>
    <xsl:value-of select="$indent"/>
    <xsl:text>}</xsl:text>
  </xsl:if>
  <xsl:text>
</xsl:text>
</xsl:template>

<!--=========================================== feature structures -->

<xsl:template match="f">
  <xsl:value-of select="@name"/><xsl:text>:</xsl:text>
  <xsl:apply-templates/>
  <xsl:call-template name="lastspace"/>
</xsl:template>

<xsl:template match="vAlt">
  <xsl:for-each select="sym">
    <xsl:apply-templates select="."/>
    <xsl:if test="position() != last()">
      <xsl:text>|</xsl:text>
    </xsl:if>
  </xsl:for-each>
</xsl:template>

<xsl:template match="sym">
  <xsl:choose>
    <xsl:when test="@value">
      <xsl:text>"</xsl:text>
      <xsl:value-of select="@value"/>
      <xsl:text>"</xsl:text>
    </xsl:when>
    <xsl:when test="@varname">
      <xsl:text>?</xsl:text>
      <xsl:value-of select="substring(@varname,2)"/>
    </xsl:when>
  </xsl:choose>
</xsl:template>

<!--==================================================== semantics -->

<xsl:template match="semantics">
  <xsl:text>semantics:[</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>]
</xsl:text>
</xsl:template>

<xsl:template match="literal">
  <xsl:apply-templates select="label"/>
  <xsl:text>:</xsl:text>
  <xsl:apply-templates select="predicate"/>
  <xsl:text>(</xsl:text>
  <xsl:for-each select="arg">
    <xsl:apply-templates/>
    <xsl:call-template name="lastspace"/>
  </xsl:for-each>
  <xsl:text>)</xsl:text>
  <xsl:call-template name="lastspace"/>
</xsl:template>

<!--======================================================== trace -->

<xsl:template match="trace">
  <xsl:text>trace:[</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>]</xsl:text>
</xsl:template>

<xsl:template match="class">
  <xsl:value-of select="."/>
  <xsl:call-template name="lastspace"/>
</xsl:template>

<!-- utility -->

<xsl:template name="lastspace">
  <xsl:if test="position() != last()">
    <xsl:text> </xsl:text>
  </xsl:if>
</xsl:template>

</xsl:stylesheet>