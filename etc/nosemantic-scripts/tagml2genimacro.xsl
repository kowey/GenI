<?xml version="1.0" encoding="UTF-8"?>

<!-- ++++++++++++++++++++++++++++++++++++ -->
<!-- Convert tagml to geni macros format -->
<!-- ++++++++++++++++++++++++++++++++++++ -->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="text"
            version="1.0"
            encoding="utf-8"
            indent="no"
            media-type="string"/>

<xsl:template match="tagml">
<xsl:text>%%
%% GENI Macro
%% This macro was automatically generated by
%% a tagml->macro script, tagml2genimacro.xsl
%% 2005
%% contact: kow@loria.fr lai@loria.fr

</xsl:text>
<xsl:apply-templates select="treeLib/tree"/>
</xsl:template>

<!-- ***** -->
<!-- TREES -->
<!-- ***** -->

<xsl:template match="tree">
  <xsl:value-of select="@id"/>
  <xsl:text>() </xsl:text>
  <xsl:choose>
  <xsl:when test="descendant::node[@type='foot']"><xsl:text>auxiliary</xsl:text></xsl:when>
  <xsl:otherwise><xsl:text>initial</xsl:text></xsl:otherwise>
  </xsl:choose>
<xsl:text>
</xsl:text>
  <xsl:apply-templates select="node"/>
  <xsl:if test="not(node/node)">
<xsl:text>
{
}
</xsl:text>
  </xsl:if>
<xsl:text>
</xsl:text>
</xsl:template>

<xsl:template match="node">
  <xsl:text>n</xsl:text><xsl:number level="multiple"/>
  <xsl:variable name="type" value="@type"/>
  <xsl:choose>
  <xsl:when test="@type='lex'">
    <xsl:text> type:lex "</xsl:text>
    <xsl:value-of select="@lex"/>
    <xsl:text>"</xsl:text>
  </xsl:when>
  <xsl:when test="@type='anchor'">
    <xsl:text> anchor</xsl:text>
  </xsl:when>
  <xsl:otherwise>
    <xsl:if test="@type='subst'">
      <xsl:text> type:subst</xsl:text>
    </xsl:if>
    <xsl:if test="@type='foot'">
      <xsl:text> type:foot</xsl:text>
    </xsl:if>
    <xsl:text> [cat:</xsl:text>
    <xsl:value-of select="@cat"/>
    <xsl:if test="narg[@cat='top']">
      <xsl:apply-templates select="narg/fs"/>
    </xsl:if>
    <xsl:text>]![cat:</xsl:text>
    <xsl:value-of select="@cat"/>
    <xsl:if test="narg[@cat='bottom']">
      <xsl:apply-templates select="narg/fs"/>
    </xsl:if>
    <xsl:text>]</xsl:text>

<!-- other fs? -->

  </xsl:otherwise>
  </xsl:choose>
<xsl:text>
</xsl:text>
  <xsl:if test="node">
<xsl:text>{
</xsl:text>
    <xsl:apply-templates select="node"/>
<xsl:text>}
</xsl:text>
  </xsl:if>

</xsl:template>

<xsl:template match="narg/fs">
  <xsl:for-each select="f">
    <xsl:text> </xsl:text>
    <xsl:value-of select="@name"/>
    <xsl:text>:</xsl:text>
    <xsl:value-of select="sym/@value"/>
  </xsl:for-each>
</xsl:template>

</xsl:stylesheet>