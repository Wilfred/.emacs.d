/* -*- Mode: java; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is Rhino code, released
 * May 6, 1999.
 *
 * The Initial Developer of the Original Code is
 * Netscape Communications Corporation.
 * Portions created by the Initial Developer are Copyright (C) 1997-1999
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Norris Boyd
 *   Roger Lawrence
 *
 * Alternatively, the contents of this file may be used under the terms of
 * the GNU General Public License Version 2 or later (the "GPL"), in which
 * case the provisions of the GPL are applicable instead of those above. If
 * you wish to allow use of your version of this file only under the terms of
 * the GPL and not to allow others to use your version of this file under the
 * MPL, indicate your decision by deleting the provisions above and replacing
 * them with the notice and other provisions required by the GPL. If you do
 * not delete the provisions above, a recipient may use your version of this
 * file under either the MPL or the GPL.
 *
 * ***** END LICENSE BLOCK ***** */

package org.mozilla.javascript;
import java.util.ArrayList;
import java.util.HashMap;

public class FunctionNode extends ScriptOrFnNode {

    public FunctionNode(String name) {
        super(Token.FUNCTION);
        functionName = name;
    }

    public String getFunctionName() {
        return functionName;
    }

    public boolean requiresActivation() {
        return itsNeedsActivation;
    }

    public boolean getIgnoreDynamicScope() {
        return itsIgnoreDynamicScope;
    }
    
    public boolean isGenerator() {
      return itsIsGenerator;
    }

    public void addResumptionPoint(Node target) {
        if (generatorResumePoints == null)
            generatorResumePoints = new ArrayList<Node>();
        generatorResumePoints.add(target);
    }

    public ArrayList<Node> getResumptionPoints() {
        return generatorResumePoints;
    }

    public HashMap<Node,int[]> getLiveLocals() {
        return liveLocals;
    }

    public void addLiveLocals(Node node, int[] locals) {
        if (liveLocals == null)
            liveLocals = new HashMap<Node,int[]>();
        liveLocals.put(node, locals);
    }

    /**
     * There are three types of functions that can be defined. The first
     * is a function statement. This is a function appearing as a top-level
     * statement (i.e., not nested inside some other statement) in either a
     * script or a function.
     *
     * The second is a function expression, which is a function appearing in
     * an expression except for the third type, which is...
     *
     * The third type is a function expression where the expression is the
     * top-level expression in an expression statement.
     *
     * The three types of functions have different treatment and must be
     * distinguished.
     */
    public static final int FUNCTION_STATEMENT            = 1;
    public static final int FUNCTION_EXPRESSION           = 2;
    public static final int FUNCTION_EXPRESSION_STATEMENT = 3;

    public int getFunctionType() {
        return itsFunctionType;
    }

    String functionName;
    int itsFunctionType;
    boolean itsNeedsActivation;
    boolean itsIgnoreDynamicScope;
    boolean itsIsGenerator;
    ArrayList<Node> generatorResumePoints;
    HashMap<Node,int[]> liveLocals;
}
