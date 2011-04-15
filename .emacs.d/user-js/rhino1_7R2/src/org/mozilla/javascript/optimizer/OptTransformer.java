/* ***** BEGIN LICENSE BLOCK *****
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


package org.mozilla.javascript.optimizer;

import org.mozilla.javascript.*;
import java.util.Map;

/**
 * This class performs node transforms to prepare for optimization.
 *
 * @see NodeTransformer
 * @author Norris Boyd
 */

class OptTransformer extends NodeTransformer {

    OptTransformer(Map<String,OptFunctionNode> possibleDirectCalls, ObjArray directCallTargets)
    {
        this.possibleDirectCalls = possibleDirectCalls;
        this.directCallTargets = directCallTargets;
    }

    @Override
    protected void visitNew(Node node, ScriptOrFnNode tree) {
        detectDirectCall(node, tree);
        super.visitNew(node, tree);
    }

    @Override
    protected void visitCall(Node node, ScriptOrFnNode tree) {
        detectDirectCall(node, tree);
        super.visitCall(node, tree);
    }

    private void detectDirectCall(Node node, ScriptOrFnNode tree)
    {
        if (tree.getType() == Token.FUNCTION) {
            Node left = node.getFirstChild();

            // count the arguments
            int argCount = 0;
            Node arg = left.getNext();
            while (arg != null) {
                arg = arg.getNext();
                argCount++;
            }

            if (argCount == 0) {
                OptFunctionNode.get(tree).itsContainsCalls0 = true;
            }

            /*
             * Optimize a call site by converting call("a", b, c) into :
             *
             *  FunctionObjectFor"a" <-- instance variable init'd by constructor
             *
             *  // this is a DIRECTCALL node
             *  fn = GetProp(tmp = GetBase("a"), "a");
             *  if (fn == FunctionObjectFor"a")
             *      fn.call(tmp, b, c)
             *  else
             *      ScriptRuntime.Call(fn, tmp, b, c)
             */
            if (possibleDirectCalls != null) {
                String targetName = null;
                if (left.getType() == Token.NAME) {
                    targetName = left.getString();
                } else if (left.getType() == Token.GETPROP) {
                    targetName = left.getFirstChild().getNext().getString();
                } else if (left.getType() == Token.GETPROPNOWARN) {
                    throw Kit.codeBug();
                }
                if (targetName != null) {
                    OptFunctionNode ofn;
                    ofn = possibleDirectCalls.get(targetName);
                    if (ofn != null
                        && argCount == ofn.fnode.getParamCount()
                        && !ofn.fnode.requiresActivation())
                    {
                        // Refuse to directCall any function with more
                        // than 32 parameters - prevent code explosion
                        // for wacky test cases
                        if (argCount <= 32) {
                            node.putProp(Node.DIRECTCALL_PROP, ofn);
                            if (!ofn.isTargetOfDirectCall()) {
                                int index = directCallTargets.size();
                                directCallTargets.add(ofn);
                                ofn.setDirectTargetIndex(index);
                            }
                        }
                    }
                }
            }
        }
    }

    private Map<String,OptFunctionNode> possibleDirectCalls;
    private ObjArray directCallTargets;
}
