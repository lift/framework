#!/bin/bash
LIFT_SCREEN_TEMPLATE=`find ./ -name "wizard-all.html"`
LIFT_SCREEN_EXTENDS=`grep -E "extends +LiftScreen"`
LIFT_SCREEN_WITH=`grep -E "with +LiftScreen"`

if [[ -n "$LIFT_SCREEN_TEMPLATE" ]]; then
  echo "You're likely using an outdated base LiftScreen template at:"
  echo "$LIFT_SCREEN_TEMPLATE"
  echo "Assuming you haven't changed it, you can replace it with the lift_basic version for"
  echo "exactly the same result:"
  echo "https://github.com/lift/lift_30_sbt/blob/master/lift_basic/src/main/webapp/templates-hidden/wizard-all.html"
  echo "----------------------------------------------------------------------------------"
fi

if [[ -n $LIFT_SCREEN_EXTENDS ]] || [[ -n $LIFT_SCREEN_WITH ]]; then
  echo "You're extending LiftScreen. LiftScreen as of Lift 3.0 is the equivalent of 2.6's"
  echo "CssBoundLiftScreen. This means it binds using CSS selector transforms instead of"
  echo "the Lift 2.x series's \`bind\` function, which no longer exists. See this document"
  echo "for porting instructions:"
  echo "https://github.com/lift/framework/docs/migration/2.6-to-3.0-lift-screen.adoc"
  echo "Here are the uses of LiftScreen we found:"
  [[ -n $LIFT_SCREEN_EXTENDS ]] && echo "$LIFT_SCREEN_EXTENDS"
  [[ -n $LIFT_SCREEN_WITH ]] && echo "$LIFT_SCREEN_WITH"
  echo "----------------------------------------------------------------------------------"
fi

BIND_USES=`grep -E "bind\("`

if [[ -n $BIND_USES ]]; then
  echo "You seem to be using Lift's bind helpers. These have been removed from Lift 3.0,"
  echo "superseded by Lift's CSS selector transforms. You can port your application to CSS"
  echo "selector transforms piecewise while still on Lift 2.6, as they are supported in"
  echo "both versions. For a primer on CSS selector transforms, look at this document:"
  echo "https://github.com/lift/framework/blob/master/docs/css-selectors.adoc"
  echo ""
  echo "If you find yourself with additional questions, please ask on the Lift mailing list"
  echo "and you should find willing helpers."
fi
