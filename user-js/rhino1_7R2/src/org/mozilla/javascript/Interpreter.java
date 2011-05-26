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
 * Portions created by the Initial Developer are Copyright (C) 1997-2000
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Patrick Beard
 *   Norris Boyd
 *   Igor Bukanov
 *   Ethan Hugg
 *   Bob Jervis
 *   Terry Lucas
 *   Roger Lawrence
 *   Milen Nankov
 *   Hannes Wallnoefer
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

import java.io.PrintStream;
import java.io.Serializable;
import java.util.List;
import java.util.ArrayList;

import org.mozilla.javascript.ScriptRuntime.NoSuchMethodShim;
import org.mozilla.javascript.debug.DebugFrame;

public class Interpreter implements Evaluator
{

// Additional interpreter-specific codes

    private static final int

    // Stack: ... value1 -> ... value1 value1
        Icode_DUP                       = -1,

    // Stack: ... value2 value1 -> ... value2 value1 value2 value1
        Icode_DUP2                      = -2,

    // Stack: ... value2 value1 -> ... value1 value2
        Icode_SWAP                      = -3,

    // Stack: ... value1 -> ...
        Icode_POP                       = -4,

    // Store stack top into return register and then pop it
        Icode_POP_RESULT                = -5,

    // To jump conditionally and pop additional stack value
        Icode_IFEQ_POP                  = -6,

    // various types of ++/--
        Icode_VAR_INC_DEC               = -7,
        Icode_NAME_INC_DEC              = -8,
        Icode_PROP_INC_DEC              = -9,
        Icode_ELEM_INC_DEC              = -10,
        Icode_REF_INC_DEC               = -11,

    // load/save scope from/to local
        Icode_SCOPE_LOAD                = -12,
        Icode_SCOPE_SAVE                = -13,

        Icode_TYPEOFNAME                = -14,

    // helper for function calls
        Icode_NAME_AND_THIS             = -15,
        Icode_PROP_AND_THIS             = -16,
        Icode_ELEM_AND_THIS             = -17,
        Icode_VALUE_AND_THIS            = -18,

    // Create closure object for nested functions
        Icode_CLOSURE_EXPR              = -19,
        Icode_CLOSURE_STMT              = -20,

    // Special calls
        Icode_CALLSPECIAL               = -21,

    // To return undefined value
        Icode_RETUNDEF                  = -22,

    // Exception handling implementation
        Icode_GOSUB                     = -23,
        Icode_STARTSUB                  = -24,
        Icode_RETSUB                    = -25,

    // To indicating a line number change in icodes.
        Icode_LINE                      = -26,

    // To store shorts and ints inline
        Icode_SHORTNUMBER               = -27,
        Icode_INTNUMBER                 = -28,

    // To create and populate array to hold values for [] and {} literals
        Icode_LITERAL_NEW               = -29,
        Icode_LITERAL_SET               = -30,

    // Array literal with skipped index like [1,,2]
        Icode_SPARE_ARRAYLIT            = -31,

    // Load index register to prepare for the following index operation
        Icode_REG_IND_C0                = -32,
        Icode_REG_IND_C1                = -33,
        Icode_REG_IND_C2                = -34,
        Icode_REG_IND_C3                = -35,
        Icode_REG_IND_C4                = -36,
        Icode_REG_IND_C5                = -37,
        Icode_REG_IND1                  = -38,
        Icode_REG_IND2                  = -39,
        Icode_REG_IND4                  = -40,

    // Load string register to prepare for the following string operation
        Icode_REG_STR_C0                = -41,
        Icode_REG_STR_C1                = -42,
        Icode_REG_STR_C2                = -43,
        Icode_REG_STR_C3                = -44,
        Icode_REG_STR1                  = -45,
        Icode_REG_STR2                  = -46,
        Icode_REG_STR4                  = -47,

    // Version of getvar/setvar that read var index directly from bytecode
        Icode_GETVAR1                   = -48,
        Icode_SETVAR1                   = -49,

    // Load unefined
        Icode_UNDEF                     = -50,
        Icode_ZERO                      = -51,
        Icode_ONE                       = -52,

    // entrance and exit from .()
       Icode_ENTERDQ                    = -53,
       Icode_LEAVEDQ                    = -54,

       Icode_TAIL_CALL                  = -55,

    // Clear local to allow GC its context
       Icode_LOCAL_CLEAR                = -56,

    // Literal get/set
       Icode_LITERAL_GETTER             = -57,
       Icode_LITERAL_SETTER             = -58,

    // const
       Icode_SETCONST                   = -59,
       Icode_SETCONSTVAR                = -60,
       Icode_SETCONSTVAR1               = -61,

    // Generator opcodes (along with Token.YIELD)
       Icode_GENERATOR                  = -62,
       Icode_GENERATOR_END              = -63,

       Icode_DEBUGGER                   = -64,

       // Last icode
        MIN_ICODE                       = -64;

    // data for parsing

    private CompilerEnvirons compilerEnv;

    private boolean itsInFunctionFlag;
    private boolean itsInTryFlag;

    private InterpreterData itsData;
    private ScriptOrFnNode scriptOrFn;
    private int itsICodeTop;
    private int itsStackDepth;
    private int itsLineNumber;
    private int itsDoubleTableTop;
    private ObjToIntMap itsStrings = new ObjToIntMap(20);
    private int itsLocalTop;

    private static final int MIN_LABEL_TABLE_SIZE = 32;
    private static final int MIN_FIXUP_TABLE_SIZE = 40;
    private int[] itsLabelTable;
    private int itsLabelTableTop;
// itsFixupTable[i] = (label_index << 32) | fixup_site
    private long[] itsFixupTable;
    private int itsFixupTableTop;
    private ObjArray itsLiteralIds = new ObjArray();

    private int itsExceptionTableTop;
    private static final int EXCEPTION_TRY_START_SLOT  = 0;
    private static final int EXCEPTION_TRY_END_SLOT    = 1;
    private static final int EXCEPTION_HANDLER_SLOT    = 2;
    private static final int EXCEPTION_TYPE_SLOT       = 3;
    private static final int EXCEPTION_LOCAL_SLOT      = 4;
    private static final int EXCEPTION_SCOPE_SLOT      = 5;
    // SLOT_SIZE: space for try start/end, handler, start, handler type,
    //            exception local and scope local
    private static final int EXCEPTION_SLOT_SIZE       = 6;

// ECF_ or Expression Context Flags constants: for now only TAIL is available
    private static final int ECF_TAIL = 1 << 0;

    /**
     * Class to hold data corresponding to one interpreted call stack frame.
     */
    private static class CallFrame implements Cloneable, Serializable
    {
        static final long serialVersionUID = -2843792508994958978L;

        CallFrame parentFrame;
        // amount of stack frames before this one on the interpretation stack
        int frameIndex;
        // If true indicates read-only frame that is a part of continuation
        boolean frozen;

        InterpretedFunction fnOrScript;
        InterpreterData idata;

// Stack structure
// stack[0 <= i < localShift]: arguments and local variables
// stack[localShift <= i <= emptyStackTop]: used for local temporaries
// stack[emptyStackTop < i < stack.length]: stack data
// sDbl[i]: if stack[i] is UniqueTag.DOUBLE_MARK, sDbl[i] holds the number value

        Object[] stack;
        int[] stackAttributes;
        double[] sDbl;
        CallFrame varSource; // defaults to this unless continuation frame
        int localShift;
        int emptyStackTop;

        DebugFrame debuggerFrame;
        boolean useActivation;
        boolean isContinuationsTopFrame;

        Scriptable thisObj;
        Scriptable[] scriptRegExps;

// The values that change during interpretation

        Object result;
        double resultDbl;
        int pc;
        int pcPrevBranch;
        int pcSourceLineStart;
        Scriptable scope;

        int savedStackTop;
        int savedCallOp;
        Object throwable;

        CallFrame cloneFrozen()
        {
            if (!frozen) Kit.codeBug();

            CallFrame copy;
            try {
                copy = (CallFrame)clone();
            } catch (CloneNotSupportedException ex) {
                throw new IllegalStateException();
            }

            // clone stack but keep varSource to point to values
            // from this frame to share variables.

            copy.stack = stack.clone();
            copy.stackAttributes = stackAttributes.clone();
            copy.sDbl = sDbl.clone();

            copy.frozen = false;
            return copy;
        }
    }

    private static final class ContinuationJump implements Serializable
    {
        static final long serialVersionUID = 7687739156004308247L;

        CallFrame capturedFrame;
        CallFrame branchFrame;
        Object result;
        double resultDbl;

        ContinuationJump(NativeContinuation c, CallFrame current)
        {
            this.capturedFrame = (CallFrame)c.getImplementation();
            if (this.capturedFrame == null || current == null) {
                // Continuation and current execution does not share
                // any frames if there is nothing to capture or
                // if there is no currently executed frames
                this.branchFrame = null;
            } else {
                // Search for branch frame where parent frame chains starting
                // from captured and current meet.
                CallFrame chain1 = this.capturedFrame;
                CallFrame chain2 = current;

                // First work parents of chain1 or chain2 until the same
                // frame depth.
                int diff = chain1.frameIndex - chain2.frameIndex;
                if (diff != 0) {
                    if (diff < 0) {
                        // swap to make sure that
                        // chain1.frameIndex > chain2.frameIndex and diff > 0
                        chain1 = current;
                        chain2 = this.capturedFrame;
                        diff = -diff;
                    }
                    do {
                        chain1 = chain1.parentFrame;
                    } while (--diff != 0);
                    if (chain1.frameIndex != chain2.frameIndex) Kit.codeBug();
                }

                // Now walk parents in parallel until a shared frame is found
                // or until the root is reached.
                while (chain1 != chain2 && chain1 != null) {
                    chain1 = chain1.parentFrame;
                    chain2 = chain2.parentFrame;
                }

                this.branchFrame = chain1;
                if (this.branchFrame != null && !this.branchFrame.frozen)
                    Kit.codeBug();
            }
        }
    }

    private static CallFrame captureFrameForGenerator(CallFrame frame) {
      frame.frozen = true;
      CallFrame result = frame.cloneFrozen();
      frame.frozen = false;

      // now isolate this frame from its previous context
      result.parentFrame = null;
      result.frameIndex = 0;

      return result;
    }

    static {
        // Checks for byte code consistencies, good compiler can eliminate them

        if (Token.LAST_BYTECODE_TOKEN > 127) {
            String str = "Violation of Token.LAST_BYTECODE_TOKEN <= 127";
            System.err.println(str);
            throw new IllegalStateException(str);
        }
        if (MIN_ICODE < -128) {
            String str = "Violation of Interpreter.MIN_ICODE >= -128";
            System.err.println(str);
            throw new IllegalStateException(str);
        }
    }

    private static String bytecodeName(int bytecode)
    {
        if (!validBytecode(bytecode)) {
            throw new IllegalArgumentException(String.valueOf(bytecode));
        }

        if (!Token.printICode) {
            return String.valueOf(bytecode);
        }

        if (validTokenCode(bytecode)) {
            return Token.name(bytecode);
        }

        switch (bytecode) {
          case Icode_DUP:              return "DUP";
          case Icode_DUP2:             return "DUP2";
          case Icode_SWAP:             return "SWAP";
          case Icode_POP:              return "POP";
          case Icode_POP_RESULT:       return "POP_RESULT";
          case Icode_IFEQ_POP:         return "IFEQ_POP";
          case Icode_VAR_INC_DEC:      return "VAR_INC_DEC";
          case Icode_NAME_INC_DEC:     return "NAME_INC_DEC";
          case Icode_PROP_INC_DEC:     return "PROP_INC_DEC";
          case Icode_ELEM_INC_DEC:     return "ELEM_INC_DEC";
          case Icode_REF_INC_DEC:      return "REF_INC_DEC";
          case Icode_SCOPE_LOAD:       return "SCOPE_LOAD";
          case Icode_SCOPE_SAVE:       return "SCOPE_SAVE";
          case Icode_TYPEOFNAME:       return "TYPEOFNAME";
          case Icode_NAME_AND_THIS:    return "NAME_AND_THIS";
          case Icode_PROP_AND_THIS:    return "PROP_AND_THIS";
          case Icode_ELEM_AND_THIS:    return "ELEM_AND_THIS";
          case Icode_VALUE_AND_THIS:   return "VALUE_AND_THIS";
          case Icode_CLOSURE_EXPR:     return "CLOSURE_EXPR";
          case Icode_CLOSURE_STMT:     return "CLOSURE_STMT";
          case Icode_CALLSPECIAL:      return "CALLSPECIAL";
          case Icode_RETUNDEF:         return "RETUNDEF";
          case Icode_GOSUB:            return "GOSUB";
          case Icode_STARTSUB:         return "STARTSUB";
          case Icode_RETSUB:           return "RETSUB";
          case Icode_LINE:             return "LINE";
          case Icode_SHORTNUMBER:      return "SHORTNUMBER";
          case Icode_INTNUMBER:        return "INTNUMBER";
          case Icode_LITERAL_NEW:      return "LITERAL_NEW";
          case Icode_LITERAL_SET:      return "LITERAL_SET";
          case Icode_SPARE_ARRAYLIT:   return "SPARE_ARRAYLIT";
          case Icode_REG_IND_C0:       return "REG_IND_C0";
          case Icode_REG_IND_C1:       return "REG_IND_C1";
          case Icode_REG_IND_C2:       return "REG_IND_C2";
          case Icode_REG_IND_C3:       return "REG_IND_C3";
          case Icode_REG_IND_C4:       return "REG_IND_C4";
          case Icode_REG_IND_C5:       return "REG_IND_C5";
          case Icode_REG_IND1:         return "LOAD_IND1";
          case Icode_REG_IND2:         return "LOAD_IND2";
          case Icode_REG_IND4:         return "LOAD_IND4";
          case Icode_REG_STR_C0:       return "REG_STR_C0";
          case Icode_REG_STR_C1:       return "REG_STR_C1";
          case Icode_REG_STR_C2:       return "REG_STR_C2";
          case Icode_REG_STR_C3:       return "REG_STR_C3";
          case Icode_REG_STR1:         return "LOAD_STR1";
          case Icode_REG_STR2:         return "LOAD_STR2";
          case Icode_REG_STR4:         return "LOAD_STR4";
          case Icode_GETVAR1:          return "GETVAR1";
          case Icode_SETVAR1:          return "SETVAR1";
          case Icode_UNDEF:            return "UNDEF";
          case Icode_ZERO:             return "ZERO";
          case Icode_ONE:              return "ONE";
          case Icode_ENTERDQ:          return "ENTERDQ";
          case Icode_LEAVEDQ:          return "LEAVEDQ";
          case Icode_TAIL_CALL:        return "TAIL_CALL";
          case Icode_LOCAL_CLEAR:      return "LOCAL_CLEAR";
          case Icode_LITERAL_GETTER:   return "LITERAL_GETTER";
          case Icode_LITERAL_SETTER:   return "LITERAL_SETTER";
          case Icode_SETCONST:         return "SETCONST";
          case Icode_SETCONSTVAR:      return "SETCONSTVAR";
          case Icode_SETCONSTVAR1:     return "SETCONSTVAR1";
          case Icode_GENERATOR:        return "GENERATOR";
          case Icode_GENERATOR_END:    return "GENERATOR_END";
          case Icode_DEBUGGER:         return "DEBUGGER";
        }

        // icode without name
        throw new IllegalStateException(String.valueOf(bytecode));
    }

    private static boolean validIcode(int icode)
    {
        return MIN_ICODE <= icode && icode <= -1;
    }

    private static boolean validTokenCode(int token)
    {
        return Token.FIRST_BYTECODE_TOKEN <= token
               && token <= Token.LAST_BYTECODE_TOKEN;
    }

    private static boolean validBytecode(int bytecode)
    {
        return validIcode(bytecode) || validTokenCode(bytecode);
    }

    public Object compile(CompilerEnvirons compilerEnv,
                          ScriptOrFnNode tree,
                          String encodedSource,
                          boolean returnFunction)
    {
        this.compilerEnv = compilerEnv;
        new NodeTransformer().transform(tree);

        if (Token.printTrees) {
            System.out.println(tree.toStringTree(tree));
        }

        if (returnFunction) {
            tree = tree.getFunctionNode(0);
        }

        scriptOrFn = tree;
        itsData = new InterpreterData(compilerEnv.getLanguageVersion(),
                                      scriptOrFn.getSourceName(),
                                      encodedSource);
        itsData.topLevel = true;

        if (returnFunction) {
            generateFunctionICode();
        } else {
            generateICodeFromTree(scriptOrFn);
        }

        return itsData;
    }

    public Script createScriptObject(Object bytecode, Object staticSecurityDomain)
    {
        if(bytecode != itsData)
        {
            Kit.codeBug();
        }
        return InterpretedFunction.createScript(itsData,
                                                staticSecurityDomain);
    }

    public void setEvalScriptFlag(Script script) {
        ((InterpretedFunction)script).idata.evalScriptFlag = true;
    }
    

    public Function createFunctionObject(Context cx, Scriptable scope,
            Object bytecode, Object staticSecurityDomain)
    {
        if(bytecode != itsData)
        {
            Kit.codeBug();
        }
        return InterpretedFunction.createFunction(cx, scope, itsData,
                                                  staticSecurityDomain);
    }

    private void generateFunctionICode()
    {
        itsInFunctionFlag = true;

        FunctionNode theFunction = (FunctionNode)scriptOrFn;

        itsData.itsFunctionType = theFunction.getFunctionType();
        itsData.itsNeedsActivation = theFunction.requiresActivation();
        itsData.itsName = theFunction.getFunctionName();
        if (!theFunction.getIgnoreDynamicScope()) {
            if (compilerEnv.isUseDynamicScope()) {
                itsData.useDynamicScope = true;
            }
        }
        if (theFunction.isGenerator()) {
          addIcode(Icode_GENERATOR);
          addUint16(theFunction.getBaseLineno() & 0xFFFF);
        }

        generateICodeFromTree(theFunction.getLastChild());
    }

    private void generateICodeFromTree(Node tree)
    {
        generateNestedFunctions();

        generateRegExpLiterals();

        visitStatement(tree, 0);
        fixLabelGotos();
        // add RETURN_RESULT only to scripts as function always ends with RETURN
        if (itsData.itsFunctionType == 0) {
            addToken(Token.RETURN_RESULT);
        }

        if (itsData.itsICode.length != itsICodeTop) {
            // Make itsData.itsICode length exactly itsICodeTop to save memory
            // and catch bugs with jumps beyond icode as early as possible
            byte[] tmp = new byte[itsICodeTop];
            System.arraycopy(itsData.itsICode, 0, tmp, 0, itsICodeTop);
            itsData.itsICode = tmp;
        }
        if (itsStrings.size() == 0) {
            itsData.itsStringTable = null;
        } else {
            itsData.itsStringTable = new String[itsStrings.size()];
            ObjToIntMap.Iterator iter = itsStrings.newIterator();
            for (iter.start(); !iter.done(); iter.next()) {
                String str = (String)iter.getKey();
                int index = iter.getValue();
                if (itsData.itsStringTable[index] != null) Kit.codeBug();
                itsData.itsStringTable[index] = str;
            }
        }
        if (itsDoubleTableTop == 0) {
            itsData.itsDoubleTable = null;
        } else if (itsData.itsDoubleTable.length != itsDoubleTableTop) {
            double[] tmp = new double[itsDoubleTableTop];
            System.arraycopy(itsData.itsDoubleTable, 0, tmp, 0,
                             itsDoubleTableTop);
            itsData.itsDoubleTable = tmp;
        }
        if (itsExceptionTableTop != 0
            && itsData.itsExceptionTable.length != itsExceptionTableTop)
        {
            int[] tmp = new int[itsExceptionTableTop];
            System.arraycopy(itsData.itsExceptionTable, 0, tmp, 0,
                             itsExceptionTableTop);
            itsData.itsExceptionTable = tmp;
        }

        itsData.itsMaxVars = scriptOrFn.getParamAndVarCount();
        // itsMaxFrameArray: interpret method needs this amount for its
        // stack and sDbl arrays
        itsData.itsMaxFrameArray = itsData.itsMaxVars
                                   + itsData.itsMaxLocals
                                   + itsData.itsMaxStack;

        itsData.argNames = scriptOrFn.getParamAndVarNames();
        itsData.argIsConst = scriptOrFn.getParamAndVarConst();
        itsData.argCount = scriptOrFn.getParamCount();

        itsData.encodedSourceStart = scriptOrFn.getEncodedSourceStart();
        itsData.encodedSourceEnd = scriptOrFn.getEncodedSourceEnd();

        if (itsLiteralIds.size() != 0) {
            itsData.literalIds = itsLiteralIds.toArray();
        }

        if (Token.printICode) dumpICode(itsData);
    }

    private void generateNestedFunctions()
    {
        int functionCount = scriptOrFn.getFunctionCount();
        if (functionCount == 0) return;

        InterpreterData[] array = new InterpreterData[functionCount];
        for (int i = 0; i != functionCount; i++) {
            FunctionNode def = scriptOrFn.getFunctionNode(i);
            Interpreter jsi = new Interpreter();
            jsi.compilerEnv = compilerEnv;
            jsi.scriptOrFn = def;
            jsi.itsData = new InterpreterData(itsData);
            jsi.generateFunctionICode();
            array[i] = jsi.itsData;
        }
        itsData.itsNestedFunctions = array;
    }

    private void generateRegExpLiterals()
    {
        int N = scriptOrFn.getRegexpCount();
        if (N == 0) return;

        Context cx = Context.getContext();
        RegExpProxy rep = ScriptRuntime.checkRegExpProxy(cx);
        Object[] array = new Object[N];
        for (int i = 0; i != N; i++) {
            String string = scriptOrFn.getRegexpString(i);
            String flags = scriptOrFn.getRegexpFlags(i);
            array[i] = rep.compileRegExp(cx, string, flags);
        }
        itsData.itsRegExpLiterals = array;
    }

    private void updateLineNumber(Node node)
    {
        int lineno = node.getLineno();
        if (lineno != itsLineNumber && lineno >= 0) {
            if (itsData.firstLinePC < 0) {
                itsData.firstLinePC = lineno;
            }
            itsLineNumber = lineno;
            addIcode(Icode_LINE);
            addUint16(lineno & 0xFFFF);
        }
    }

    private RuntimeException badTree(Node node)
    {
        throw new RuntimeException(node.toString());
    }

    private void visitStatement(Node node, int initialStackDepth)
    {
        int type = node.getType();
        Node child = node.getFirstChild();
        switch (type) {

          case Token.FUNCTION:
            {
                int fnIndex = node.getExistingIntProp(Node.FUNCTION_PROP);
                int fnType = scriptOrFn.getFunctionNode(fnIndex).
                                 getFunctionType();
                // Only function expressions or function expression
                // statements need closure code creating new function
                // object on stack as function statements are initialized
                // at script/function start.
                // In addition, function expressions can not be present here
                // at statement level, they must only be present as expressions.
                if (fnType == FunctionNode.FUNCTION_EXPRESSION_STATEMENT) {
                    addIndexOp(Icode_CLOSURE_STMT, fnIndex);
                } else {
                    if (fnType != FunctionNode.FUNCTION_STATEMENT) {
                        throw Kit.codeBug();
                    }
                }
                // For function statements or function expression statements
                // in scripts, we need to ensure that the result of the script
                // is the function if it is the last statement in the script.
                // For example, eval("function () {}") should return a
                // function, not undefined.
                if (!itsInFunctionFlag) {
                    addIndexOp(Icode_CLOSURE_EXPR, fnIndex);
                    stackChange(1);
                    addIcode(Icode_POP_RESULT);
                    stackChange(-1);
                }
            }
            break;

          case Token.LABEL:
          case Token.LOOP:
          case Token.BLOCK:
          case Token.EMPTY:
          case Token.WITH:
            updateLineNumber(node);
          case Token.SCRIPT:
            // fall through
            while (child != null) {
                visitStatement(child, initialStackDepth);
                child = child.getNext();
            }
            break;

          case Token.ENTERWITH:
            visitExpression(child, 0);
            addToken(Token.ENTERWITH);
            stackChange(-1);
            break;

          case Token.LEAVEWITH:
            addToken(Token.LEAVEWITH);
            break;

          case Token.LOCAL_BLOCK:
            {
                int local = allocLocal();
                node.putIntProp(Node.LOCAL_PROP, local);
                updateLineNumber(node);
                while (child != null) {
                    visitStatement(child, initialStackDepth);
                    child = child.getNext();
                }
                addIndexOp(Icode_LOCAL_CLEAR, local);
                releaseLocal(local);
            }
            break;

          case Token.DEBUGGER:
            addIcode(Icode_DEBUGGER);
            break;

          case Token.SWITCH:
            updateLineNumber(node);
            // See comments in IRFactory.createSwitch() for description
            // of SWITCH node
            {
                visitExpression(child, 0);
                for (Node.Jump caseNode = (Node.Jump)child.getNext();
                     caseNode != null;
                     caseNode = (Node.Jump)caseNode.getNext())
                {
                    if (caseNode.getType() != Token.CASE)
                        throw badTree(caseNode);
                    Node test = caseNode.getFirstChild();
                    addIcode(Icode_DUP);
                    stackChange(1);
                    visitExpression(test, 0);
                    addToken(Token.SHEQ);
                    stackChange(-1);
                    // If true, Icode_IFEQ_POP will jump and remove case
                    // value from stack
                    addGoto(caseNode.target, Icode_IFEQ_POP);
                    stackChange(-1);
                }
                addIcode(Icode_POP);
                stackChange(-1);
            }
            break;

          case Token.TARGET:
            markTargetLabel(node);
            break;

          case Token.IFEQ :
          case Token.IFNE :
            {
                Node target = ((Node.Jump)node).target;
                visitExpression(child, 0);
                addGoto(target, type);
                stackChange(-1);
            }
            break;

          case Token.GOTO:
            {
                Node target = ((Node.Jump)node).target;
                addGoto(target, type);
            }
            break;

          case Token.JSR:
            {
                Node target = ((Node.Jump)node).target;
                addGoto(target, Icode_GOSUB);
            }
            break;

          case Token.FINALLY:
            {
                // Account for incomming GOTOSUB address
                stackChange(1);
                int finallyRegister = getLocalBlockRef(node);
                addIndexOp(Icode_STARTSUB, finallyRegister);
                stackChange(-1);
                while (child != null) {
                    visitStatement(child, initialStackDepth);
                    child = child.getNext();
                }
                addIndexOp(Icode_RETSUB, finallyRegister);
            }
            break;

          case Token.EXPR_VOID:
          case Token.EXPR_RESULT:
            updateLineNumber(node);
            visitExpression(child, 0);
            addIcode((type == Token.EXPR_VOID) ? Icode_POP : Icode_POP_RESULT);
            stackChange(-1);
            break;

          case Token.TRY:
            {
                Node.Jump tryNode = (Node.Jump)node;
                int exceptionObjectLocal = getLocalBlockRef(tryNode);
                int scopeLocal = allocLocal();

                addIndexOp(Icode_SCOPE_SAVE, scopeLocal);

                int tryStart = itsICodeTop;
                boolean savedFlag = itsInTryFlag;
                itsInTryFlag = true;
                while (child != null) {
                    visitStatement(child, initialStackDepth);
                    child = child.getNext();
                }
                itsInTryFlag = savedFlag;

                Node catchTarget = tryNode.target;
                if (catchTarget != null) {
                    int catchStartPC
                        = itsLabelTable[getTargetLabel(catchTarget)];
                    addExceptionHandler(
                        tryStart, catchStartPC, catchStartPC,
                        false, exceptionObjectLocal, scopeLocal);
                }
                Node finallyTarget = tryNode.getFinally();
                if (finallyTarget != null) {
                    int finallyStartPC
                        = itsLabelTable[getTargetLabel(finallyTarget)];
                    addExceptionHandler(
                        tryStart, finallyStartPC, finallyStartPC,
                        true, exceptionObjectLocal, scopeLocal);
                }

                addIndexOp(Icode_LOCAL_CLEAR, scopeLocal);
                releaseLocal(scopeLocal);
            }
            break;

          case Token.CATCH_SCOPE:
            {
                int localIndex = getLocalBlockRef(node);
                int scopeIndex = node.getExistingIntProp(Node.CATCH_SCOPE_PROP);
                String name = child.getString();
                child = child.getNext();
                visitExpression(child, 0); // load expression object
                addStringPrefix(name);
                addIndexPrefix(localIndex);
                addToken(Token.CATCH_SCOPE);
                addUint8(scopeIndex != 0 ? 1 : 0);
                stackChange(-1);
            }
            break;

          case Token.THROW:
            updateLineNumber(node);
            visitExpression(child, 0);
            addToken(Token.THROW);
            addUint16(itsLineNumber & 0xFFFF);
            stackChange(-1);
            break;

          case Token.RETHROW:
            updateLineNumber(node);
            addIndexOp(Token.RETHROW, getLocalBlockRef(node));
            break;

          case Token.RETURN:
            updateLineNumber(node);
            if (node.getIntProp(Node.GENERATOR_END_PROP, 0) != 0) {
                // We're in a generator, so change RETURN to GENERATOR_END
                addIcode(Icode_GENERATOR_END);
                addUint16(itsLineNumber & 0xFFFF);
            } else if (child != null) {
                visitExpression(child, ECF_TAIL);
                addToken(Token.RETURN);
                stackChange(-1);
            } else {
                addIcode(Icode_RETUNDEF);
            }
            break;

          case Token.RETURN_RESULT:
            updateLineNumber(node);
            addToken(Token.RETURN_RESULT);
            break;

          case Token.ENUM_INIT_KEYS:
          case Token.ENUM_INIT_VALUES:
          case Token.ENUM_INIT_ARRAY:
            visitExpression(child, 0);
            addIndexOp(type, getLocalBlockRef(node));
            stackChange(-1);
            break;

          case Icode_GENERATOR:
            break;

          default:
            throw badTree(node);
        }

        if (itsStackDepth != initialStackDepth) {
            throw Kit.codeBug();
        }
    }

    private void visitExpression(Node node, int contextFlags)
    {
        int type = node.getType();
        Node child = node.getFirstChild();
        int savedStackDepth = itsStackDepth;
        switch (type) {

          case Token.FUNCTION:
            {
                int fnIndex = node.getExistingIntProp(Node.FUNCTION_PROP);
                FunctionNode fn = scriptOrFn.getFunctionNode(fnIndex);
                // See comments in visitStatement for Token.FUNCTION case
                if (fn.getFunctionType() != FunctionNode.FUNCTION_EXPRESSION) {
                    throw Kit.codeBug();
                }
                addIndexOp(Icode_CLOSURE_EXPR, fnIndex);
                stackChange(1);
            }
            break;

          case Token.LOCAL_LOAD:
            {
                int localIndex = getLocalBlockRef(node);
                addIndexOp(Token.LOCAL_LOAD, localIndex);
                stackChange(1);
            }
            break;

          case Token.COMMA:
            {
                Node lastChild = node.getLastChild();
                while (child != lastChild) {
                    visitExpression(child, 0);
                    addIcode(Icode_POP);
                    stackChange(-1);
                    child = child.getNext();
                }
                // Preserve tail context flag if any
                visitExpression(child, contextFlags & ECF_TAIL);
            }
            break;

          case Token.USE_STACK:
            // Indicates that stack was modified externally,
            // like placed catch object
            stackChange(1);
            break;

          case Token.REF_CALL:
          case Token.CALL:
          case Token.NEW:
            {
                if (type == Token.NEW) {
                    visitExpression(child, 0);
                } else {
                    generateCallFunAndThis(child);
                }
                int argCount = 0;
                while ((child = child.getNext()) != null) {
                    visitExpression(child, 0);
                    ++argCount;
                }
                int callType = node.getIntProp(Node.SPECIALCALL_PROP,
                                               Node.NON_SPECIALCALL);
                if (callType != Node.NON_SPECIALCALL) {
                    // embed line number and source filename
                    addIndexOp(Icode_CALLSPECIAL, argCount);
                    addUint8(callType);
                    addUint8(type == Token.NEW ? 1 : 0);
                    addUint16(itsLineNumber & 0xFFFF);
                } else {
                    // Only use the tail call optimization if we're not in a try
                    // or we're not generating debug info (since the
                    // optimization will confuse the debugger)
                    if (type == Token.CALL && (contextFlags & ECF_TAIL) != 0 &&
                        !compilerEnv.isGenerateDebugInfo() && !itsInTryFlag)
                    {
                        type = Icode_TAIL_CALL;
                    }
                    addIndexOp(type, argCount);
                }
                // adjust stack
                if (type == Token.NEW) {
                    // new: f, args -> result
                    stackChange(-argCount);
                } else {
                    // call: f, thisObj, args -> result
                    // ref_call: f, thisObj, args -> ref
                    stackChange(-1 - argCount);
                }
                if (argCount > itsData.itsMaxCalleeArgs) {
                    itsData.itsMaxCalleeArgs = argCount;
                }
            }
            break;

          case Token.AND:
          case Token.OR:
            {
                visitExpression(child, 0);
                addIcode(Icode_DUP);
                stackChange(1);
                int afterSecondJumpStart = itsICodeTop;
                int jump = (type == Token.AND) ? Token.IFNE : Token.IFEQ;
                addGotoOp(jump);
                stackChange(-1);
                addIcode(Icode_POP);
                stackChange(-1);
                child = child.getNext();
                // Preserve tail context flag if any
                visitExpression(child, contextFlags & ECF_TAIL);
                resolveForwardGoto(afterSecondJumpStart);
            }
            break;

          case Token.HOOK:
            {
                Node ifThen = child.getNext();
                Node ifElse = ifThen.getNext();
                visitExpression(child, 0);
                int elseJumpStart = itsICodeTop;
                addGotoOp(Token.IFNE);
                stackChange(-1);
                // Preserve tail context flag if any
                visitExpression(ifThen, contextFlags & ECF_TAIL);
                int afterElseJumpStart = itsICodeTop;
                addGotoOp(Token.GOTO);
                resolveForwardGoto(elseJumpStart);
                itsStackDepth = savedStackDepth;
                // Preserve tail context flag if any
                visitExpression(ifElse, contextFlags & ECF_TAIL);
                resolveForwardGoto(afterElseJumpStart);
            }
            break;

          case Token.GETPROP:
          case Token.GETPROPNOWARN:
            visitExpression(child, 0);
            child = child.getNext();
            addStringOp(type, child.getString());
            break;

          case Token.GETELEM:
          case Token.DELPROP:
          case Token.BITAND:
          case Token.BITOR:
          case Token.BITXOR:
          case Token.LSH:
          case Token.RSH:
          case Token.URSH:
          case Token.ADD:
          case Token.SUB:
          case Token.MOD:
          case Token.DIV:
          case Token.MUL:
          case Token.EQ:
          case Token.NE:
          case Token.SHEQ:
          case Token.SHNE:
          case Token.IN:
          case Token.INSTANCEOF:
          case Token.LE:
          case Token.LT:
          case Token.GE:
          case Token.GT:
            visitExpression(child, 0);
            child = child.getNext();
            visitExpression(child, 0);
            addToken(type);
            stackChange(-1);
            break;

          case Token.POS:
          case Token.NEG:
          case Token.NOT:
          case Token.BITNOT:
          case Token.TYPEOF:
          case Token.VOID:
            visitExpression(child, 0);
            if (type == Token.VOID) {
                addIcode(Icode_POP);
                addIcode(Icode_UNDEF);
            } else {
                addToken(type);
            }
            break;

          case Token.GET_REF:
          case Token.DEL_REF:
            visitExpression(child, 0);
            addToken(type);
            break;

          case Token.SETPROP:
          case Token.SETPROP_OP:
            {
                visitExpression(child, 0);
                child = child.getNext();
                String property = child.getString();
                child = child.getNext();
                if (type == Token.SETPROP_OP) {
                    addIcode(Icode_DUP);
                    stackChange(1);
                    addStringOp(Token.GETPROP, property);
                    // Compensate for the following USE_STACK
                    stackChange(-1);
                }
                visitExpression(child, 0);
                addStringOp(Token.SETPROP, property);
                stackChange(-1);
            }
            break;

          case Token.SETELEM:
          case Token.SETELEM_OP:
            visitExpression(child, 0);
            child = child.getNext();
            visitExpression(child, 0);
            child = child.getNext();
            if (type == Token.SETELEM_OP) {
                addIcode(Icode_DUP2);
                stackChange(2);
                addToken(Token.GETELEM);
                stackChange(-1);
                // Compensate for the following USE_STACK
                stackChange(-1);
            }
            visitExpression(child, 0);
            addToken(Token.SETELEM);
            stackChange(-2);
            break;

          case Token.SET_REF:
          case Token.SET_REF_OP:
            visitExpression(child, 0);
            child = child.getNext();
            if (type == Token.SET_REF_OP) {
                addIcode(Icode_DUP);
                stackChange(1);
                addToken(Token.GET_REF);
                // Compensate for the following USE_STACK
                stackChange(-1);
            }
            visitExpression(child, 0);
            addToken(Token.SET_REF);
            stackChange(-1);
            break;

          case Token.SETNAME:
            {
                String name = child.getString();
                visitExpression(child, 0);
                child = child.getNext();
                visitExpression(child, 0);
                addStringOp(Token.SETNAME, name);
                stackChange(-1);
            }
            break;

          case Token.SETCONST:
            {
                String name = child.getString();
                visitExpression(child, 0);
                child = child.getNext();
                visitExpression(child, 0);
                addStringOp(Icode_SETCONST, name);
                stackChange(-1);
            }
            break;

          case Token.TYPEOFNAME:
            {
                int index = -1;
                // use typeofname if an activation frame exists
                // since the vars all exist there instead of in jregs
                if (itsInFunctionFlag && !itsData.itsNeedsActivation)
                    index = scriptOrFn.getIndexForNameNode(node);
                if (index == -1) {
                    addStringOp(Icode_TYPEOFNAME, node.getString());
                    stackChange(1);
                } else {
                    addVarOp(Token.GETVAR, index);
                    stackChange(1);
                    addToken(Token.TYPEOF);
                }
            }
            break;

          case Token.BINDNAME:
          case Token.NAME:
          case Token.STRING:
            addStringOp(type, node.getString());
            stackChange(1);
            break;

          case Token.INC:
          case Token.DEC:
            visitIncDec(node, child);
            break;

          case Token.NUMBER:
            {
                double num = node.getDouble();
                int inum = (int)num;
                if (inum == num) {
                    if (inum == 0) {
                        addIcode(Icode_ZERO);
                        // Check for negative zero
                        if (1.0 / num < 0.0) {
                            addToken(Token.NEG);
                        }
                    } else if (inum == 1) {
                        addIcode(Icode_ONE);
                    } else if ((short)inum == inum) {
                        addIcode(Icode_SHORTNUMBER);
                        // write short as uin16 bit pattern
                        addUint16(inum & 0xFFFF);
                    } else {
                        addIcode(Icode_INTNUMBER);
                        addInt(inum);
                    }
                } else {
                    int index = getDoubleIndex(num);
                    addIndexOp(Token.NUMBER, index);
                }
                stackChange(1);
            }
            break;

          case Token.GETVAR:
            {
                if (itsData.itsNeedsActivation) Kit.codeBug();
                int index = scriptOrFn.getIndexForNameNode(node);
                addVarOp(Token.GETVAR, index);
                stackChange(1);
            }
            break;

          case Token.SETVAR:
            {
                if (itsData.itsNeedsActivation) Kit.codeBug();
                int index = scriptOrFn.getIndexForNameNode(child);
                child = child.getNext();
                visitExpression(child, 0);
                addVarOp(Token.SETVAR, index);
            }
            break;

          case Token.SETCONSTVAR:
            {
                if (itsData.itsNeedsActivation) Kit.codeBug();
                int index = scriptOrFn.getIndexForNameNode(child);
                child = child.getNext();
                visitExpression(child, 0);
                addVarOp(Token.SETCONSTVAR, index);
            }
            break;

          case Token.NULL:
          case Token.THIS:
          case Token.THISFN:
          case Token.FALSE:
          case Token.TRUE:
            addToken(type);
            stackChange(1);
            break;

          case Token.ENUM_NEXT:
          case Token.ENUM_ID:
            addIndexOp(type, getLocalBlockRef(node));
            stackChange(1);
            break;

          case Token.REGEXP:
            {
                int index = node.getExistingIntProp(Node.REGEXP_PROP);
                addIndexOp(Token.REGEXP, index);
                stackChange(1);
            }
            break;

          case Token.ARRAYLIT:
          case Token.OBJECTLIT:
            visitLiteral(node, child);
            break;

          case Token.ARRAYCOMP:
            visitArrayComprehension(node, child, child.getNext());
            break;

          case Token.REF_SPECIAL:
            visitExpression(child, 0);
            addStringOp(type, (String)node.getProp(Node.NAME_PROP));
            break;

          case Token.REF_MEMBER:
          case Token.REF_NS_MEMBER:
          case Token.REF_NAME:
          case Token.REF_NS_NAME:
            {
                int memberTypeFlags = node.getIntProp(Node.MEMBER_TYPE_PROP, 0);
                // generate possible target, possible namespace and member
                int childCount = 0;
                do {
                    visitExpression(child, 0);
                    ++childCount;
                    child = child.getNext();
                } while (child != null);
                addIndexOp(type, memberTypeFlags);
                stackChange(1 - childCount);
            }
            break;

          case Token.DOTQUERY:
            {
                int queryPC;
                updateLineNumber(node);
                visitExpression(child, 0);
                addIcode(Icode_ENTERDQ);
                stackChange(-1);
                queryPC = itsICodeTop;
                visitExpression(child.getNext(), 0);
                addBackwardGoto(Icode_LEAVEDQ, queryPC);
            }
            break;

          case Token.DEFAULTNAMESPACE :
          case Token.ESCXMLATTR :
          case Token.ESCXMLTEXT :
            visitExpression(child, 0);
            addToken(type);
            break;

          case Token.YIELD:
            if (child != null) {
                visitExpression(child, 0);
            } else {
                addIcode(Icode_UNDEF);
                stackChange(1);
            }
            addToken(Token.YIELD);
            addUint16(node.getLineno() & 0xFFFF);
            break;

          case Token.WITHEXPR: {
            Node enterWith = node.getFirstChild();
            Node with = enterWith.getNext();
            visitExpression(enterWith.getFirstChild(), 0);
            addToken(Token.ENTERWITH);
            stackChange(-1);
            visitExpression(with.getFirstChild(), 0);
            addToken(Token.LEAVEWITH);
            break;
          }

          default:
            throw badTree(node);
        }
        if (savedStackDepth + 1 != itsStackDepth) {
            Kit.codeBug();
        }
    }

    private void generateCallFunAndThis(Node left)
    {
        // Generate code to place on stack function and thisObj
        int type = left.getType();
        switch (type) {
          case Token.NAME: {
            String name = left.getString();
            // stack: ... -> ... function thisObj
            addStringOp(Icode_NAME_AND_THIS, name);
            stackChange(2);
            break;
          }
          case Token.GETPROP:
          case Token.GETELEM: {
            Node target = left.getFirstChild();
            visitExpression(target, 0);
            Node id = target.getNext();
            if (type == Token.GETPROP) {
                String property = id.getString();
                // stack: ... target -> ... function thisObj
                addStringOp(Icode_PROP_AND_THIS, property);
                stackChange(1);
            } else {
                visitExpression(id, 0);
                // stack: ... target id -> ... function thisObj
                addIcode(Icode_ELEM_AND_THIS);
            }
            break;
          }
          default:
            // Including Token.GETVAR
            visitExpression(left, 0);
            // stack: ... value -> ... function thisObj
            addIcode(Icode_VALUE_AND_THIS);
            stackChange(1);
            break;
        }
    }

    private void visitIncDec(Node node, Node child)
    {
        int incrDecrMask = node.getExistingIntProp(Node.INCRDECR_PROP);
        int childType = child.getType();
        switch (childType) {
          case Token.GETVAR : {
            if (itsData.itsNeedsActivation) Kit.codeBug();
            int i = scriptOrFn.getIndexForNameNode(child);
            addVarOp(Icode_VAR_INC_DEC, i);
            addUint8(incrDecrMask);
            stackChange(1);
            break;
          }
          case Token.NAME : {
            String name = child.getString();
            addStringOp(Icode_NAME_INC_DEC, name);
            addUint8(incrDecrMask);
            stackChange(1);
            break;
          }
          case Token.GETPROP : {
            Node object = child.getFirstChild();
            visitExpression(object, 0);
            String property = object.getNext().getString();
            addStringOp(Icode_PROP_INC_DEC, property);
            addUint8(incrDecrMask);
            break;
          }
          case Token.GETELEM : {
            Node object = child.getFirstChild();
            visitExpression(object, 0);
            Node index = object.getNext();
            visitExpression(index, 0);
            addIcode(Icode_ELEM_INC_DEC);
            addUint8(incrDecrMask);
            stackChange(-1);
            break;
          }
          case Token.GET_REF : {
            Node ref = child.getFirstChild();
            visitExpression(ref, 0);
            addIcode(Icode_REF_INC_DEC);
            addUint8(incrDecrMask);
            break;
          }
          default : {
            throw badTree(node);
          }
        }
    }

    private void visitLiteral(Node node, Node child)
    {
        int type = node.getType();
        int count;
        Object[] propertyIds = null;
        if (type == Token.ARRAYLIT) {
            count = 0;
            for (Node n = child; n != null; n = n.getNext()) {
                ++count;
            }
        } else if (type == Token.OBJECTLIT) {
            propertyIds = (Object[])node.getProp(Node.OBJECT_IDS_PROP);
            count = propertyIds.length;
        } else {
            throw badTree(node);
        }
        addIndexOp(Icode_LITERAL_NEW, count);
        stackChange(2);
        while (child != null) {
            int childType = child.getType();
            if (childType == Token.GET) {
                visitExpression(child.getFirstChild(), 0);
                addIcode(Icode_LITERAL_GETTER);
            } else if (childType == Token.SET) {
                visitExpression(child.getFirstChild(), 0);
                addIcode(Icode_LITERAL_SETTER);
            } else {
                visitExpression(child, 0);
                addIcode(Icode_LITERAL_SET);
            }
            stackChange(-1);
            child = child.getNext();
        }
        if (type == Token.ARRAYLIT) {
            int[] skipIndexes = (int[])node.getProp(Node.SKIP_INDEXES_PROP);
            if (skipIndexes == null) {
                addToken(Token.ARRAYLIT);
            } else {
                int index = itsLiteralIds.size();
                itsLiteralIds.add(skipIndexes);
                addIndexOp(Icode_SPARE_ARRAYLIT, index);
            }
        } else {
            int index = itsLiteralIds.size();
            itsLiteralIds.add(propertyIds);
            addIndexOp(Token.OBJECTLIT, index);
        }
        stackChange(-1);
    }
    
    private void visitArrayComprehension(Node node, Node initStmt, Node expr)
    {
        // A bit of a hack: array comprehensions are implemented using
        // statement nodes for the iteration, yet they appear in an
        // expression context. So we pass the current stack depth to
        // visitStatement so it can check that the depth is not altered
        // by statements.
        visitStatement(initStmt, itsStackDepth);
        visitExpression(expr, 0);
    }

    private int getLocalBlockRef(Node node)
    {
        Node localBlock = (Node)node.getProp(Node.LOCAL_BLOCK_PROP);
        return localBlock.getExistingIntProp(Node.LOCAL_PROP);
    }

    private int getTargetLabel(Node target)
    {
        int label = target.labelId();
        if (label != -1) {
            return label;
        }
        label = itsLabelTableTop;
        if (itsLabelTable == null || label == itsLabelTable.length) {
            if (itsLabelTable == null) {
                itsLabelTable = new int[MIN_LABEL_TABLE_SIZE];
            }else {
                int[] tmp = new int[itsLabelTable.length * 2];
                System.arraycopy(itsLabelTable, 0, tmp, 0, label);
                itsLabelTable = tmp;
            }
        }
        itsLabelTableTop = label + 1;
        itsLabelTable[label] = -1;

        target.labelId(label);
        return label;
    }

    private void markTargetLabel(Node target)
    {
        int label = getTargetLabel(target);
        if (itsLabelTable[label] != -1) {
            // Can mark label only once
            Kit.codeBug();
        }
        itsLabelTable[label] = itsICodeTop;
    }

    private void addGoto(Node target, int gotoOp)
    {
        int label = getTargetLabel(target);
        if (!(label < itsLabelTableTop)) Kit.codeBug();
        int targetPC = itsLabelTable[label];

        if (targetPC != -1) {
            addBackwardGoto(gotoOp, targetPC);
        } else {
            int gotoPC = itsICodeTop;
            addGotoOp(gotoOp);
            int top = itsFixupTableTop;
            if (itsFixupTable == null || top == itsFixupTable.length) {
                if (itsFixupTable == null) {
                    itsFixupTable = new long[MIN_FIXUP_TABLE_SIZE];
                } else {
                    long[] tmp = new long[itsFixupTable.length * 2];
                    System.arraycopy(itsFixupTable, 0, tmp, 0, top);
                    itsFixupTable = tmp;
                }
            }
            itsFixupTableTop = top + 1;
            itsFixupTable[top] = ((long)label << 32) | gotoPC;
        }
    }

    private void fixLabelGotos()
    {
        for (int i = 0; i < itsFixupTableTop; i++) {
            long fixup = itsFixupTable[i];
            int label = (int)(fixup >> 32);
            int jumpSource = (int)fixup;
            int pc = itsLabelTable[label];
            if (pc == -1) {
                // Unlocated label
                throw Kit.codeBug();
            }
            resolveGoto(jumpSource, pc);
        }
        itsFixupTableTop = 0;
    }

    private void addBackwardGoto(int gotoOp, int jumpPC)
    {
        int fromPC = itsICodeTop;
        // Ensure that this is a jump backward
        if (fromPC <= jumpPC) throw Kit.codeBug();
        addGotoOp(gotoOp);
        resolveGoto(fromPC, jumpPC);
    }

    private void resolveForwardGoto(int fromPC)
    {
        // Ensure that forward jump skips at least self bytecode
        if (itsICodeTop < fromPC + 3) throw Kit.codeBug();
        resolveGoto(fromPC, itsICodeTop);
    }

    private void resolveGoto(int fromPC, int jumpPC)
    {
        int offset = jumpPC - fromPC;
        // Ensure that jumps do not overlap
        if (0 <= offset && offset <= 2) throw Kit.codeBug();
        int offsetSite = fromPC + 1;
        if (offset != (short)offset) {
            if (itsData.longJumps == null) {
                itsData.longJumps = new UintMap();
            }
            itsData.longJumps.put(offsetSite, jumpPC);
            offset = 0;
        }
        byte[] array = itsData.itsICode;
        array[offsetSite] = (byte)(offset >> 8);
        array[offsetSite + 1] = (byte)offset;
    }

    private void addToken(int token)
    {
        if (!validTokenCode(token)) throw Kit.codeBug();
        addUint8(token);
    }

    private void addIcode(int icode)
    {
        if (!validIcode(icode)) throw Kit.codeBug();
        // Write negative icode as uint8 bits
        addUint8(icode & 0xFF);
    }

    private void addUint8(int value)
    {
        if ((value & ~0xFF) != 0) throw Kit.codeBug();
        byte[] array = itsData.itsICode;
        int top = itsICodeTop;
        if (top == array.length) {
            array = increaseICodeCapacity(1);
        }
        array[top] = (byte)value;
        itsICodeTop = top + 1;
    }

    private void addUint16(int value)
    {
        if ((value & ~0xFFFF) != 0) throw Kit.codeBug();
        byte[] array = itsData.itsICode;
        int top = itsICodeTop;
        if (top + 2 > array.length) {
            array = increaseICodeCapacity(2);
        }
        array[top] = (byte)(value >>> 8);
        array[top + 1] = (byte)value;
        itsICodeTop = top + 2;
    }

    private void addInt(int i)
    {
        byte[] array = itsData.itsICode;
        int top = itsICodeTop;
        if (top + 4 > array.length) {
            array = increaseICodeCapacity(4);
        }
        array[top] = (byte)(i >>> 24);
        array[top + 1] = (byte)(i >>> 16);
        array[top + 2] = (byte)(i >>> 8);
        array[top + 3] = (byte)i;
        itsICodeTop = top + 4;
    }

    private int getDoubleIndex(double num)
    {
        int index = itsDoubleTableTop;
        if (index == 0) {
            itsData.itsDoubleTable = new double[64];
        } else if (itsData.itsDoubleTable.length == index) {
            double[] na = new double[index * 2];
            System.arraycopy(itsData.itsDoubleTable, 0, na, 0, index);
            itsData.itsDoubleTable = na;
        }
        itsData.itsDoubleTable[index] = num;
        itsDoubleTableTop = index + 1;
        return index;
    }

    private void addGotoOp(int gotoOp)
    {
        byte[] array = itsData.itsICode;
        int top = itsICodeTop;
        if (top + 3 > array.length) {
            array = increaseICodeCapacity(3);
        }
        array[top] = (byte)gotoOp;
        // Offset would written later
        itsICodeTop = top + 1 + 2;
    }

    private void addVarOp(int op, int varIndex)
    {
        switch (op) {
          case Token.SETCONSTVAR:
            if (varIndex < 128) {
                addIcode(Icode_SETCONSTVAR1);
                addUint8(varIndex);
                return;
            }
            addIndexOp(Icode_SETCONSTVAR, varIndex);
            return;
          case Token.GETVAR:
          case Token.SETVAR:
            if (varIndex < 128) {
                addIcode(op == Token.GETVAR ? Icode_GETVAR1 : Icode_SETVAR1);
                addUint8(varIndex);
                return;
            }
            // fallthrough
          case Icode_VAR_INC_DEC:
            addIndexOp(op, varIndex);
            return;
        }
        throw Kit.codeBug();
    }

    private void addStringOp(int op, String str)
    {
        addStringPrefix(str);
        if (validIcode(op)) {
            addIcode(op);
        } else {
            addToken(op);
        }
    }

    private void addIndexOp(int op, int index)
    {
        addIndexPrefix(index);
        if (validIcode(op)) {
            addIcode(op);
        } else {
            addToken(op);
        }
    }

    private void addStringPrefix(String str)
    {
        int index = itsStrings.get(str, -1);
        if (index == -1) {
            index = itsStrings.size();
            itsStrings.put(str, index);
        }
        if (index < 4) {
            addIcode(Icode_REG_STR_C0 - index);
        } else if (index <= 0xFF) {
            addIcode(Icode_REG_STR1);
            addUint8(index);
         } else if (index <= 0xFFFF) {
            addIcode(Icode_REG_STR2);
            addUint16(index);
         } else {
            addIcode(Icode_REG_STR4);
            addInt(index);
        }
    }

    private void addIndexPrefix(int index)
    {
        if (index < 0) Kit.codeBug();
        if (index < 6) {
            addIcode(Icode_REG_IND_C0 - index);
        } else if (index <= 0xFF) {
            addIcode(Icode_REG_IND1);
            addUint8(index);
         } else if (index <= 0xFFFF) {
            addIcode(Icode_REG_IND2);
            addUint16(index);
         } else {
            addIcode(Icode_REG_IND4);
            addInt(index);
        }
    }

    private void addExceptionHandler(int icodeStart, int icodeEnd,
                                     int handlerStart, boolean isFinally,
                                     int exceptionObjectLocal, int scopeLocal)
    {
        int top = itsExceptionTableTop;
        int[] table = itsData.itsExceptionTable;
        if (table == null) {
            if (top != 0) Kit.codeBug();
            table = new int[EXCEPTION_SLOT_SIZE * 2];
            itsData.itsExceptionTable = table;
        } else if (table.length == top) {
            table = new int[table.length * 2];
            System.arraycopy(itsData.itsExceptionTable, 0, table, 0, top);
            itsData.itsExceptionTable = table;
        }
        table[top + EXCEPTION_TRY_START_SLOT]  = icodeStart;
        table[top + EXCEPTION_TRY_END_SLOT]    = icodeEnd;
        table[top + EXCEPTION_HANDLER_SLOT]    = handlerStart;
        table[top + EXCEPTION_TYPE_SLOT]       = isFinally ? 1 : 0;
        table[top + EXCEPTION_LOCAL_SLOT]      = exceptionObjectLocal;
        table[top + EXCEPTION_SCOPE_SLOT]      = scopeLocal;

        itsExceptionTableTop = top + EXCEPTION_SLOT_SIZE;
    }

    private byte[] increaseICodeCapacity(int extraSize)
    {
        int capacity = itsData.itsICode.length;
        int top = itsICodeTop;
        if (top + extraSize <= capacity) throw Kit.codeBug();
        capacity *= 2;
        if (top + extraSize > capacity) {
            capacity = top + extraSize;
        }
        byte[] array = new byte[capacity];
        System.arraycopy(itsData.itsICode, 0, array, 0, top);
        itsData.itsICode = array;
        return array;
    }

    private void stackChange(int change)
    {
        if (change <= 0) {
            itsStackDepth += change;
        } else {
            int newDepth = itsStackDepth + change;
            if (newDepth > itsData.itsMaxStack) {
                itsData.itsMaxStack = newDepth;
            }
            itsStackDepth = newDepth;
        }
    }

    private int allocLocal()
    {
        int localSlot = itsLocalTop;
        ++itsLocalTop;
        if (itsLocalTop > itsData.itsMaxLocals) {
            itsData.itsMaxLocals = itsLocalTop;
        }
        return localSlot;
    }

    private void releaseLocal(int localSlot)
    {
        --itsLocalTop;
        if (localSlot != itsLocalTop) Kit.codeBug();
    }

    private static int getShort(byte[] iCode, int pc) {
        return (iCode[pc] << 8) | (iCode[pc + 1] & 0xFF);
    }

    private static int getIndex(byte[] iCode, int pc) {
        return ((iCode[pc] & 0xFF) << 8) | (iCode[pc + 1] & 0xFF);
    }

    private static int getInt(byte[] iCode, int pc) {
        return (iCode[pc] << 24) | ((iCode[pc + 1] & 0xFF) << 16)
               | ((iCode[pc + 2] & 0xFF) << 8) | (iCode[pc + 3] & 0xFF);
    }

    private static int getExceptionHandler(CallFrame frame,
                                           boolean onlyFinally)
    {
        int[] exceptionTable = frame.idata.itsExceptionTable;
        if (exceptionTable == null) {
            // No exception handlers
            return -1;
        }

        // Icode switch in the interpreter increments PC immediately
        // and it is necessary to subtract 1 from the saved PC
        // to point it before the start of the next instruction.
        int pc = frame.pc - 1;

        // OPT: use binary search
        int best = -1, bestStart = 0, bestEnd = 0;
        for (int i = 0; i != exceptionTable.length; i += EXCEPTION_SLOT_SIZE) {
            int start = exceptionTable[i + EXCEPTION_TRY_START_SLOT];
            int end = exceptionTable[i + EXCEPTION_TRY_END_SLOT];
            if (!(start <= pc && pc < end)) {
                continue;
            }
            if (onlyFinally && exceptionTable[i + EXCEPTION_TYPE_SLOT] != 1) {
                continue;
            }
            if (best >= 0) {
                // Since handlers always nest and they never have shared end
                // although they can share start  it is sufficient to compare
                // handlers ends
                if (bestEnd < end) {
                    continue;
                }
                // Check the above assumption
                if (bestStart > start) Kit.codeBug(); // should be nested
                if (bestEnd == end) Kit.codeBug();  // no ens sharing
            }
            best = i;
            bestStart = start;
            bestEnd = end;
        }
        return best;
    }

    private static void dumpICode(InterpreterData idata)
    {
        if (!Token.printICode) {
            return;
        }

        byte iCode[] = idata.itsICode;
        int iCodeLength = iCode.length;
        String[] strings = idata.itsStringTable;
        PrintStream out = System.out;
        out.println("ICode dump, for " + idata.itsName
                    + ", length = " + iCodeLength);
        out.println("MaxStack = " + idata.itsMaxStack);

        int indexReg = 0;
        for (int pc = 0; pc < iCodeLength; ) {
            out.flush();
            out.print(" [" + pc + "] ");
            int token = iCode[pc];
            int icodeLength = bytecodeSpan(token);
            String tname = bytecodeName(token);
            int old_pc = pc;
            ++pc;
            switch (token) {
              default:
                if (icodeLength != 1) Kit.codeBug();
                out.println(tname);
                break;

              case Icode_GOSUB :
              case Token.GOTO :
              case Token.IFEQ :
              case Token.IFNE :
              case Icode_IFEQ_POP :
              case Icode_LEAVEDQ : {
                int newPC = pc + getShort(iCode, pc) - 1;
                out.println(tname + " " + newPC);
                pc += 2;
                break;
              }
              case Icode_VAR_INC_DEC :
              case Icode_NAME_INC_DEC :
              case Icode_PROP_INC_DEC :
              case Icode_ELEM_INC_DEC :
              case Icode_REF_INC_DEC: {
                int incrDecrType = iCode[pc];
                out.println(tname + " " + incrDecrType);
                ++pc;
                break;
              }

              case Icode_CALLSPECIAL : {
                int callType = iCode[pc] & 0xFF;
                boolean isNew =  (iCode[pc + 1] != 0);
                int line = getIndex(iCode, pc+2);
                out.println(tname+" "+callType+" "+isNew+" "+indexReg+" "+line);
                pc += 4;
                break;
              }

              case Token.CATCH_SCOPE:
                {
                    boolean afterFisrtFlag =  (iCode[pc] != 0);
                    out.println(tname+" "+afterFisrtFlag);
                    ++pc;
                }
                break;
              case Token.REGEXP :
                out.println(tname+" "+idata.itsRegExpLiterals[indexReg]);
                break;
              case Token.OBJECTLIT :
              case Icode_SPARE_ARRAYLIT :
                out.println(tname+" "+idata.literalIds[indexReg]);
                break;
              case Icode_CLOSURE_EXPR :
              case Icode_CLOSURE_STMT :
                out.println(tname+" "+idata.itsNestedFunctions[indexReg]);
                break;
              case Token.CALL :
              case Icode_TAIL_CALL :
              case Token.REF_CALL :
              case Token.NEW :
                out.println(tname+' '+indexReg);
                break;
              case Token.THROW :
              case Token.YIELD :
              case Icode_GENERATOR :
              case Icode_GENERATOR_END :
              {
                int line = getIndex(iCode, pc);
                out.println(tname + " : " + line);
                pc += 2;
                break;
              }
              case Icode_SHORTNUMBER : {
                int value = getShort(iCode, pc);
                out.println(tname + " " + value);
                pc += 2;
                break;
              }
              case Icode_INTNUMBER : {
                int value = getInt(iCode, pc);
                out.println(tname + " " + value);
                pc += 4;
                break;
              }
              case Token.NUMBER : {
                double value = idata.itsDoubleTable[indexReg];
                out.println(tname + " " + value);
                break;
              }
              case Icode_LINE : {
                int line = getIndex(iCode, pc);
                out.println(tname + " : " + line);
                pc += 2;
                break;
              }
              case Icode_REG_STR1: {
                String str = strings[0xFF & iCode[pc]];
                out.println(tname + " \"" + str + '"');
                ++pc;
                break;
              }
              case Icode_REG_STR2: {
                String str = strings[getIndex(iCode, pc)];
                out.println(tname + " \"" + str + '"');
                pc += 2;
                break;
              }
              case Icode_REG_STR4: {
                String str = strings[getInt(iCode, pc)];
                out.println(tname + " \"" + str + '"');
                pc += 4;
                break;
              }
              case Icode_REG_IND_C0:
                  indexReg = 0;
                  out.println(tname);
                  break;
              case Icode_REG_IND_C1:
                  indexReg = 1;
                  out.println(tname);
                  break;
              case Icode_REG_IND_C2:
                  indexReg = 2;
                  out.println(tname);
                  break;
              case Icode_REG_IND_C3:
                  indexReg = 3;
                  out.println(tname);
                  break;
              case Icode_REG_IND_C4:
                  indexReg = 4;
                  out.println(tname);
                  break;
              case Icode_REG_IND_C5:
                  indexReg = 5;
                  out.println(tname);
                  break;
              case Icode_REG_IND1: {
                indexReg = 0xFF & iCode[pc];
                out.println(tname+" "+indexReg);
                ++pc;
                break;
              }
              case Icode_REG_IND2: {
                indexReg = getIndex(iCode, pc);
                out.println(tname+" "+indexReg);
                pc += 2;
                break;
              }
              case Icode_REG_IND4: {
                indexReg = getInt(iCode, pc);
                out.println(tname+" "+indexReg);
                pc += 4;
                break;
              }
              case Icode_GETVAR1:
              case Icode_SETVAR1:
              case Icode_SETCONSTVAR1:
                indexReg = iCode[pc];
                out.println(tname+" "+indexReg);
                ++pc;
                break;
            }
            if (old_pc + icodeLength != pc) Kit.codeBug();
        }

        int[] table = idata.itsExceptionTable;
        if (table != null) {
            out.println("Exception handlers: "
                         +table.length / EXCEPTION_SLOT_SIZE);
            for (int i = 0; i != table.length;
                 i += EXCEPTION_SLOT_SIZE)
            {
                int tryStart       = table[i + EXCEPTION_TRY_START_SLOT];
                int tryEnd         = table[i + EXCEPTION_TRY_END_SLOT];
                int handlerStart   = table[i + EXCEPTION_HANDLER_SLOT];
                int type           = table[i + EXCEPTION_TYPE_SLOT];
                int exceptionLocal = table[i + EXCEPTION_LOCAL_SLOT];
                int scopeLocal     = table[i + EXCEPTION_SCOPE_SLOT];

                out.println(" tryStart="+tryStart+" tryEnd="+tryEnd
                            +" handlerStart="+handlerStart
                            +" type="+(type == 0 ? "catch" : "finally")
                            +" exceptionLocal="+exceptionLocal);
            }
        }
        out.flush();
    }

    private static int bytecodeSpan(int bytecode)
    {
        switch (bytecode) {
            case Token.THROW :
            case Token.YIELD:
            case Icode_GENERATOR:
            case Icode_GENERATOR_END:
                // source line
                return 1 + 2;

            case Icode_GOSUB :
            case Token.GOTO :
            case Token.IFEQ :
            case Token.IFNE :
            case Icode_IFEQ_POP :
            case Icode_LEAVEDQ :
                // target pc offset
                return 1 + 2;

            case Icode_CALLSPECIAL :
                // call type
                // is new
                // line number
                return 1 + 1 + 1 + 2;

            case Token.CATCH_SCOPE:
                // scope flag
                return 1 + 1;

            case Icode_VAR_INC_DEC:
            case Icode_NAME_INC_DEC:
            case Icode_PROP_INC_DEC:
            case Icode_ELEM_INC_DEC:
            case Icode_REF_INC_DEC:
                // type of ++/--
                return 1 + 1;

            case Icode_SHORTNUMBER :
                // short number
                return 1 + 2;

            case Icode_INTNUMBER :
                // int number
                return 1 + 4;

            case Icode_REG_IND1:
                // ubyte index
                return 1 + 1;

            case Icode_REG_IND2:
                // ushort index
                return 1 + 2;

            case Icode_REG_IND4:
                // int index
                return 1 + 4;

            case Icode_REG_STR1:
                // ubyte string index
                return 1 + 1;

            case Icode_REG_STR2:
                // ushort string index
                return 1 + 2;

            case Icode_REG_STR4:
                // int string index
                return 1 + 4;

            case Icode_GETVAR1:
            case Icode_SETVAR1:
            case Icode_SETCONSTVAR1:
                // byte var index
                return 1 + 1;

            case Icode_LINE :
                // line number
                return 1 + 2;
        }
        if (!validBytecode(bytecode)) throw Kit.codeBug();
        return 1;
    }

    static int[] getLineNumbers(InterpreterData data)
    {
        UintMap presentLines = new UintMap();

        byte[] iCode = data.itsICode;
        int iCodeLength = iCode.length;
        for (int pc = 0; pc != iCodeLength;) {
            int bytecode = iCode[pc];
            int span = bytecodeSpan(bytecode);
            if (bytecode == Icode_LINE) {
                if (span != 3) Kit.codeBug();
                int line = getIndex(iCode, pc + 1);
                presentLines.put(line, 0);
            }
            pc += span;
        }

        return presentLines.getKeys();
    }

    public void captureStackInfo(RhinoException ex)
    {
        Context cx = Context.getCurrentContext();
        if (cx == null || cx.lastInterpreterFrame == null) {
            // No interpreter invocations
            ex.interpreterStackInfo = null;
            ex.interpreterLineData = null;
            return;
        }
        // has interpreter frame on the stack
        CallFrame[] array;
        if (cx.previousInterpreterInvocations == null
            || cx.previousInterpreterInvocations.size() == 0)
        {
            array = new CallFrame[1];
        } else {
            int previousCount = cx.previousInterpreterInvocations.size();
            if (cx.previousInterpreterInvocations.peek()
                == cx.lastInterpreterFrame)
            {
                // It can happen if exception was generated after
                // frame was pushed to cx.previousInterpreterInvocations
                // but before assignment to cx.lastInterpreterFrame.
                // In this case frames has to be ignored.
                --previousCount;
            }
            array = new CallFrame[previousCount + 1];
            cx.previousInterpreterInvocations.toArray(array);
        }
        array[array.length - 1]  = (CallFrame)cx.lastInterpreterFrame;

        int interpreterFrameCount = 0;
        for (int i = 0; i != array.length; ++i) {
            interpreterFrameCount += 1 + array[i].frameIndex;
        }

        int[] linePC = new int[interpreterFrameCount];
        // Fill linePC with pc positions from all interpreter frames.
        // Start from the most nested frame
        int linePCIndex = interpreterFrameCount;
        for (int i = array.length; i != 0;) {
            --i;
            CallFrame frame = array[i];
            while (frame != null) {
                --linePCIndex;
                linePC[linePCIndex] = frame.pcSourceLineStart;
                frame = frame.parentFrame;
            }
        }
        if (linePCIndex != 0) Kit.codeBug();

        ex.interpreterStackInfo = array;
        ex.interpreterLineData = linePC;
    }

    public String getSourcePositionFromStack(Context cx, int[] linep)
    {
        CallFrame frame = (CallFrame)cx.lastInterpreterFrame;
        InterpreterData idata = frame.idata;
        if (frame.pcSourceLineStart >= 0) {
            linep[0] = getIndex(idata.itsICode, frame.pcSourceLineStart);
        } else {
            linep[0] = 0;
        }
        return idata.itsSourceFile;
    }

    public String getPatchedStack(RhinoException ex,
                                  String nativeStackTrace)
    {
        String tag = "org.mozilla.javascript.Interpreter.interpretLoop";
        StringBuffer sb = new StringBuffer(nativeStackTrace.length() + 1000);
        String lineSeparator = SecurityUtilities.getSystemProperty("line.separator");

        CallFrame[] array = (CallFrame[])ex.interpreterStackInfo;
        int[] linePC = ex.interpreterLineData;
        int arrayIndex = array.length;
        int linePCIndex = linePC.length;
        int offset = 0;
        while (arrayIndex != 0) {
            --arrayIndex;
            int pos = nativeStackTrace.indexOf(tag, offset);
            if (pos < 0) {
                break;
            }

            // Skip tag length
            pos += tag.length();
            // Skip until the end of line
            for (; pos != nativeStackTrace.length(); ++pos) {
                char c = nativeStackTrace.charAt(pos);
                if (c == '\n' || c == '\r') {
                    break;
                }
            }
            sb.append(nativeStackTrace.substring(offset, pos));
            offset = pos;

            CallFrame frame = array[arrayIndex];
            while (frame != null) {
                if (linePCIndex == 0) Kit.codeBug();
                --linePCIndex;
                InterpreterData idata = frame.idata;
                sb.append(lineSeparator);
                sb.append("\tat script");
                if (idata.itsName != null && idata.itsName.length() != 0) {
                    sb.append('.');
                    sb.append(idata.itsName);
                }
                sb.append('(');
                sb.append(idata.itsSourceFile);
                int pc = linePC[linePCIndex];
                if (pc >= 0) {
                    // Include line info only if available
                    sb.append(':');
                    sb.append(getIndex(idata.itsICode, pc));
                }
                sb.append(')');
                frame = frame.parentFrame;
            }
        }
        sb.append(nativeStackTrace.substring(offset));

        return sb.toString();
    }

    public List<String> getScriptStack(RhinoException ex)
    {
        if (ex.interpreterStackInfo == null) {
            return null;
        }
        
        List<String> list = new ArrayList<String>();
        String lineSeparator =
                SecurityUtilities.getSystemProperty("line.separator");

        CallFrame[] array = (CallFrame[])ex.interpreterStackInfo;
        int[] linePC = ex.interpreterLineData;
        int arrayIndex = array.length;
        int linePCIndex = linePC.length;
        while (arrayIndex != 0) {
            --arrayIndex;
            StringBuilder sb = new StringBuilder();
            CallFrame frame = array[arrayIndex];
            while (frame != null) {
                if (linePCIndex == 0) Kit.codeBug();
                --linePCIndex;
                InterpreterData idata = frame.idata;
                sb.append("\tat ");
                sb.append(idata.itsSourceFile);
                int pc = linePC[linePCIndex];
                if (pc >= 0) {
                    // Include line info only if available
                    sb.append(':');
                    sb.append(getIndex(idata.itsICode, pc));
                }
                if (idata.itsName != null && idata.itsName.length() != 0) {
                    sb.append(" (");
                    sb.append(idata.itsName);
                    sb.append(')');
                }
                sb.append(lineSeparator);
                frame = frame.parentFrame;
            }
            list.add(sb.toString());
        }
        return list;
    }
        
    static String getEncodedSource(InterpreterData idata)
    {
        if (idata.encodedSource == null) {
            return null;
        }
        return idata.encodedSource.substring(idata.encodedSourceStart,
                                             idata.encodedSourceEnd);
    }

    private static void initFunction(Context cx, Scriptable scope,
                                     InterpretedFunction parent, int index)
    {
        InterpretedFunction fn;
        fn = InterpretedFunction.createFunction(cx, scope, parent, index);
        ScriptRuntime.initFunction(cx, scope, fn, fn.idata.itsFunctionType,
                                   parent.idata.evalScriptFlag);
    }

    static Object interpret(InterpretedFunction ifun,
                            Context cx, Scriptable scope,
                            Scriptable thisObj, Object[] args)
    {
        if (!ScriptRuntime.hasTopCall(cx)) Kit.codeBug();

        if (cx.interpreterSecurityDomain != ifun.securityDomain) {
            Object savedDomain = cx.interpreterSecurityDomain;
            cx.interpreterSecurityDomain = ifun.securityDomain;
            try {
                return ifun.securityController.callWithDomain(
                    ifun.securityDomain, cx, ifun, scope, thisObj, args);
            } finally {
                cx.interpreterSecurityDomain = savedDomain;
            }
        }

        CallFrame frame = new CallFrame();
        initFrame(cx, scope, thisObj, args, null, 0, args.length,
                  ifun, null, frame);
        frame.isContinuationsTopFrame = cx.isContinuationsTopCall;
        cx.isContinuationsTopCall = false;

        return interpretLoop(cx, frame, null);
    }

    static class GeneratorState {
        GeneratorState(int operation, Object value) {
            this.operation = operation;
            this.value = value;
        }
        int operation;
        Object value;
        RuntimeException returnedException;
    }

    public static Object resumeGenerator(Context cx,
                                         Scriptable scope,
                                         int operation,
                                         Object savedState,
                                         Object value)
    {
      CallFrame frame = (CallFrame) savedState;
      GeneratorState generatorState = new GeneratorState(operation, value);
      if (operation == NativeGenerator.GENERATOR_CLOSE) {
          try {
              return interpretLoop(cx, frame, generatorState);
          } catch (RuntimeException e) {
              // Only propagate exceptions other than closingException
              if (e != value)
                  throw e;
          }
          return Undefined.instance;
      }
      Object result = interpretLoop(cx, frame, generatorState);
      if (generatorState.returnedException != null)
          throw generatorState.returnedException;
      return result;
    }

    public static Object restartContinuation(NativeContinuation c, Context cx,
                                             Scriptable scope, Object[] args)
    {
        if (!ScriptRuntime.hasTopCall(cx)) {
            return ScriptRuntime.doTopCall(c, cx, scope, null, args);
        }

        Object arg;
        if (args.length == 0) {
            arg = Undefined.instance;
        } else {
            arg = args[0];
        }

        CallFrame capturedFrame = (CallFrame)c.getImplementation();
        if (capturedFrame == null) {
            // No frames to restart
            return arg;
        }

        ContinuationJump cjump = new ContinuationJump(c, null);

        cjump.result = arg;
        return interpretLoop(cx, null, cjump);
    }

    private static Object interpretLoop(Context cx, CallFrame frame,
                                        Object throwable)
    {
        // throwable holds exception object to rethrow or catch
        // It is also used for continuation restart in which case
        // it holds ContinuationJump

        final Object DBL_MRK = UniqueTag.DOUBLE_MARK;
        final Object undefined = Undefined.instance;

        final boolean instructionCounting = (cx.instructionThreshold != 0);
        // arbitrary number to add to instructionCount when calling
        // other functions
        final int INVOCATION_COST = 100;
        // arbitrary exception cost for instruction counting
        final int EXCEPTION_COST = 100;

        String stringReg = null;
        int indexReg = -1;

        if (cx.lastInterpreterFrame != null) {
            // save the top frame from the previous interpretLoop
            // invocation on the stack
            if (cx.previousInterpreterInvocations == null) {
                cx.previousInterpreterInvocations = new ObjArray();
            }
            cx.previousInterpreterInvocations.push(cx.lastInterpreterFrame);
        }

        // When restarting continuation throwable is not null and to jump
        // to the code that rewind continuation state indexReg should be set
        // to -1.
        // With the normal call throwable == null and indexReg == -1 allows to
        // catch bugs with using indeReg to access array elements before
        // initializing indexReg.

        GeneratorState generatorState = null;
        if (throwable != null) {
            if (throwable instanceof GeneratorState) {
              generatorState = (GeneratorState) throwable;

              // reestablish this call frame
              enterFrame(cx, frame, ScriptRuntime.emptyArgs, true);
              throwable = null;
            } else if (!(throwable instanceof ContinuationJump)) {
                // It should be continuation
                Kit.codeBug();
            }
        }

        Object interpreterResult = null;
        double interpreterResultDbl = 0.0;

        StateLoop: for (;;) {
            withoutExceptions: try {

                if (throwable != null) {
                    // Need to return both 'frame' and 'throwable' from
                    // 'processThrowable', so just added a 'throwable'
                    // member in 'frame'.
                    frame = processThrowable(cx, throwable, frame, indexReg,
                                             instructionCounting);
                    throwable = frame.throwable;
                    frame.throwable = null;
                } else {
                    if (generatorState == null && frame.frozen) Kit.codeBug();
                }

                // Use local variables for constant values in frame
                // for faster access
                Object[] stack = frame.stack;
                double[] sDbl = frame.sDbl;
                Object[] vars = frame.varSource.stack;
                double[] varDbls = frame.varSource.sDbl;
                int[] varAttributes = frame.varSource.stackAttributes;
                byte[] iCode = frame.idata.itsICode;
                String[] strings = frame.idata.itsStringTable;

                // Use local for stackTop as well. Since execption handlers
                // can only exist at statement level where stack is empty,
                // it is necessary to save/restore stackTop only across
                // function calls and normal returns.
                int stackTop = frame.savedStackTop;

                // Store new frame in cx which is used for error reporting etc.
                cx.lastInterpreterFrame = frame;

                Loop: for (;;) {

                    // Exception handler assumes that PC is already incremented
                    // pass the instruction start when it searches the
                    // exception handler
                    int op = iCode[frame.pc++];
                    jumplessRun: {

    // Back indent to ease implementation reading
switch (op) {
    case Icode_GENERATOR: {
        if (!frame.frozen) {
          // First time encountering this opcode: create new generator
          // object and return
          frame.pc--; // we want to come back here when we resume
          CallFrame generatorFrame = captureFrameForGenerator(frame);
          generatorFrame.frozen = true;
          NativeGenerator generator = new NativeGenerator(frame.scope, 
              generatorFrame.fnOrScript, generatorFrame);
          frame.result = generator;
          break Loop;
        } else {
          // We are now resuming execution. Fall through to YIELD case.
        }
    }
    // fall through...
    case Token.YIELD: {
        if (!frame.frozen) {
            return freezeGenerator(cx, frame, stackTop, generatorState);
        } else {
            Object obj = thawGenerator(frame, stackTop, generatorState, op);
            if (obj != Scriptable.NOT_FOUND) {
                throwable = obj;
                break withoutExceptions;
            }
            continue Loop;
        }
    }
    case Icode_GENERATOR_END: {
      // throw StopIteration
      frame.frozen = true;
      int sourceLine = getIndex(iCode, frame.pc);
      generatorState.returnedException = new JavaScriptException(
          NativeIterator.getStopIterationObject(frame.scope),
          frame.idata.itsSourceFile, sourceLine);
      break Loop;
    }
    case Token.THROW: {
        Object value = stack[stackTop];
        if (value == DBL_MRK) value = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;

        int sourceLine = getIndex(iCode, frame.pc);
        throwable = new JavaScriptException(value,
                                            frame.idata.itsSourceFile,
                                            sourceLine);
        break withoutExceptions;
    }
    case Token.RETHROW: {
        indexReg += frame.localShift;
        throwable = stack[indexReg];
        break withoutExceptions;
    }
    case Token.GE :
    case Token.LE :
    case Token.GT :
    case Token.LT : {
        --stackTop;
        Object rhs = stack[stackTop + 1];
        Object lhs = stack[stackTop];
        boolean valBln;
      object_compare:
        {
          number_compare:
            {
                double rDbl, lDbl;
                if (rhs == DBL_MRK) {
                    rDbl = sDbl[stackTop + 1];
                    lDbl = stack_double(frame, stackTop);
                } else if (lhs == DBL_MRK) {
                    rDbl = ScriptRuntime.toNumber(rhs);
                    lDbl = sDbl[stackTop];
                } else {
                    break number_compare;
                }
                switch (op) {
                  case Token.GE:
                    valBln = (lDbl >= rDbl);
                    break object_compare;
                  case Token.LE:
                    valBln = (lDbl <= rDbl);
                    break object_compare;
                  case Token.GT:
                    valBln = (lDbl > rDbl);
                    break object_compare;
                  case Token.LT:
                    valBln = (lDbl < rDbl);
                    break object_compare;
                  default:
                    throw Kit.codeBug();
                }
            }
            switch (op) {
              case Token.GE:
                valBln = ScriptRuntime.cmp_LE(rhs, lhs);
                break;
              case Token.LE:
                valBln = ScriptRuntime.cmp_LE(lhs, rhs);
                break;
              case Token.GT:
                valBln = ScriptRuntime.cmp_LT(rhs, lhs);
                break;
              case Token.LT:
                valBln = ScriptRuntime.cmp_LT(lhs, rhs);
                break;
              default:
                throw Kit.codeBug();
            }
        }
        stack[stackTop] = ScriptRuntime.wrapBoolean(valBln);
        continue Loop;
    }
    case Token.IN :
    case Token.INSTANCEOF : {
        Object rhs = stack[stackTop];
        if (rhs == DBL_MRK) rhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;
        Object lhs = stack[stackTop];
        if (lhs == DBL_MRK) lhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        boolean valBln;
        if (op == Token.IN) {
            valBln = ScriptRuntime.in(lhs, rhs, cx);
        } else {
            valBln = ScriptRuntime.instanceOf(lhs, rhs, cx);
        }
        stack[stackTop] = ScriptRuntime.wrapBoolean(valBln);
        continue Loop;
    }
    case Token.EQ :
    case Token.NE : {
        --stackTop;
        boolean valBln;
        Object rhs = stack[stackTop + 1];
        Object lhs = stack[stackTop];
        if (rhs == DBL_MRK) {
            if (lhs == DBL_MRK) {
                valBln = (sDbl[stackTop] == sDbl[stackTop + 1]);
            } else {
                valBln = ScriptRuntime.eqNumber(sDbl[stackTop + 1], lhs);
            }
        } else {
            if (lhs == DBL_MRK) {
                valBln = ScriptRuntime.eqNumber(sDbl[stackTop], rhs);
            } else {
                valBln = ScriptRuntime.eq(lhs, rhs);
            }
        }
        valBln ^= (op == Token.NE);
        stack[stackTop] = ScriptRuntime.wrapBoolean(valBln);
        continue Loop;
    }
    case Token.SHEQ :
    case Token.SHNE : {
        --stackTop;
        boolean valBln = shallowEquals(stack, sDbl, stackTop);
        valBln ^= (op == Token.SHNE);
        stack[stackTop] = ScriptRuntime.wrapBoolean(valBln);
        continue Loop;
    }
    case Token.IFNE :
        if (stack_boolean(frame, stackTop--)) {
            frame.pc += 2;
            continue Loop;
        }
        break jumplessRun;
    case Token.IFEQ :
        if (!stack_boolean(frame, stackTop--)) {
            frame.pc += 2;
            continue Loop;
        }
        break jumplessRun;
    case Icode_IFEQ_POP :
        if (!stack_boolean(frame, stackTop--)) {
            frame.pc += 2;
            continue Loop;
        }
        stack[stackTop--] = null;
        break jumplessRun;
    case Token.GOTO :
        break jumplessRun;
    case Icode_GOSUB :
        ++stackTop;
        stack[stackTop] = DBL_MRK;
        sDbl[stackTop] = frame.pc + 2;
        break jumplessRun;
    case Icode_STARTSUB :
        if (stackTop == frame.emptyStackTop + 1) {
            // Call from Icode_GOSUB: store return PC address in the local
            indexReg += frame.localShift;
            stack[indexReg] = stack[stackTop];
            sDbl[indexReg] = sDbl[stackTop];
            --stackTop;
        } else {
            // Call from exception handler: exception object is already stored
            // in the local
            if (stackTop != frame.emptyStackTop) Kit.codeBug();
        }
        continue Loop;
    case Icode_RETSUB : {
        // indexReg: local to store return address
        if (instructionCounting) {
            addInstructionCount(cx, frame, 0);
        }
        indexReg += frame.localShift;
        Object value = stack[indexReg];
        if (value != DBL_MRK) {
            // Invocation from exception handler, restore object to rethrow
            throwable = value;
            break withoutExceptions;
        }
        // Normal return from GOSUB
        frame.pc = (int)sDbl[indexReg];
        if (instructionCounting) {
            frame.pcPrevBranch = frame.pc;
        }
        continue Loop;
    }
    case Icode_POP :
        stack[stackTop] = null;
        stackTop--;
        continue Loop;
    case Icode_POP_RESULT :
        frame.result = stack[stackTop];
        frame.resultDbl = sDbl[stackTop];
        stack[stackTop] = null;
        --stackTop;
        continue Loop;
    case Icode_DUP :
        stack[stackTop + 1] = stack[stackTop];
        sDbl[stackTop + 1] = sDbl[stackTop];
        stackTop++;
        continue Loop;
    case Icode_DUP2 :
        stack[stackTop + 1] = stack[stackTop - 1];
        sDbl[stackTop + 1] = sDbl[stackTop - 1];
        stack[stackTop + 2] = stack[stackTop];
        sDbl[stackTop + 2] = sDbl[stackTop];
        stackTop += 2;
        continue Loop;
    case Icode_SWAP : {
        Object o = stack[stackTop];
        stack[stackTop] = stack[stackTop - 1];
        stack[stackTop - 1] = o;
        double d = sDbl[stackTop];
        sDbl[stackTop] = sDbl[stackTop - 1];
        sDbl[stackTop - 1] = d;
        continue Loop;
    }
    case Token.RETURN :
        frame.result = stack[stackTop];
        frame.resultDbl = sDbl[stackTop];
        --stackTop;
        break Loop;
    case Token.RETURN_RESULT :
        break Loop;
    case Icode_RETUNDEF :
        frame.result = undefined;
        break Loop;
    case Token.BITNOT : {
        int rIntValue = stack_int32(frame, stackTop);
        stack[stackTop] = DBL_MRK;
        sDbl[stackTop] = ~rIntValue;
        continue Loop;
    }
    case Token.BITAND :
    case Token.BITOR :
    case Token.BITXOR :
    case Token.LSH :
    case Token.RSH : {
        int lIntValue = stack_int32(frame, stackTop-1);
        int rIntValue = stack_int32(frame, stackTop);
        stack[--stackTop] = DBL_MRK;
        switch (op) {
          case Token.BITAND:
            lIntValue &= rIntValue;
            break;
          case Token.BITOR:
            lIntValue |= rIntValue;
            break;
          case Token.BITXOR:
            lIntValue ^= rIntValue;
            break;
          case Token.LSH:
            lIntValue <<= rIntValue;
            break;
          case Token.RSH:
            lIntValue >>= rIntValue;
            break;
        }
        sDbl[stackTop] = lIntValue;
        continue Loop;
    }
    case Token.URSH : {
        double lDbl = stack_double(frame, stackTop-1);
        int rIntValue = stack_int32(frame, stackTop) & 0x1F;
        stack[--stackTop] = DBL_MRK;
        sDbl[stackTop] = ScriptRuntime.toUint32(lDbl) >>> rIntValue;
        continue Loop;
    }
    case Token.NEG :
    case Token.POS : {
        double rDbl = stack_double(frame, stackTop);
        stack[stackTop] = DBL_MRK;
        if (op == Token.NEG) {
            rDbl = -rDbl;
        }
        sDbl[stackTop] = rDbl;
        continue Loop;
    }
    case Token.ADD :
        --stackTop;
        do_add(stack, sDbl, stackTop, cx);
        continue Loop;
    case Token.SUB :
    case Token.MUL :
    case Token.DIV :
    case Token.MOD : {
        double rDbl = stack_double(frame, stackTop);
        --stackTop;
        double lDbl = stack_double(frame, stackTop);
        stack[stackTop] = DBL_MRK;
        switch (op) {
          case Token.SUB:
            lDbl -= rDbl;
            break;
          case Token.MUL:
            lDbl *= rDbl;
            break;
          case Token.DIV:
            lDbl /= rDbl;
            break;
          case Token.MOD:
            lDbl %= rDbl;
            break;
        }
        sDbl[stackTop] = lDbl;
        continue Loop;
    }
    case Token.NOT :
        stack[stackTop] = ScriptRuntime.wrapBoolean(
                              !stack_boolean(frame, stackTop));
        continue Loop;
    case Token.BINDNAME :
        stack[++stackTop] = ScriptRuntime.bind(cx, frame.scope, stringReg);
        continue Loop;
    case Token.SETNAME : {
        Object rhs = stack[stackTop];
        if (rhs == DBL_MRK) rhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;
        Scriptable lhs = (Scriptable)stack[stackTop];
        stack[stackTop] = ScriptRuntime.setName(lhs, rhs, cx,
                                                frame.scope, stringReg);
        continue Loop;
    }
    case Icode_SETCONST: {
        Object rhs = stack[stackTop];
        if (rhs == DBL_MRK) rhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;
        Scriptable lhs = (Scriptable)stack[stackTop];
        stack[stackTop] = ScriptRuntime.setConst(lhs, rhs, cx, stringReg);
        continue Loop;
    }
    case Token.DELPROP : {
        Object rhs = stack[stackTop];
        if (rhs == DBL_MRK) rhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;
        Object lhs = stack[stackTop];
        if (lhs == DBL_MRK) lhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        stack[stackTop] = ScriptRuntime.delete(lhs, rhs, cx);
        continue Loop;
    }
    case Token.GETPROPNOWARN : {
        Object lhs = stack[stackTop];
        if (lhs == DBL_MRK) lhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        stack[stackTop] = ScriptRuntime.getObjectPropNoWarn(lhs, stringReg, cx);
        continue Loop;
    }
    case Token.GETPROP : {
        Object lhs = stack[stackTop];
        if (lhs == DBL_MRK) lhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        stack[stackTop] = ScriptRuntime.getObjectProp(lhs, stringReg, cx, frame.scope);
        continue Loop;
    }
    case Token.SETPROP : {
        Object rhs = stack[stackTop];
        if (rhs == DBL_MRK) rhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;
        Object lhs = stack[stackTop];
        if (lhs == DBL_MRK) lhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        stack[stackTop] = ScriptRuntime.setObjectProp(lhs, stringReg, rhs,
                                                      cx);
        continue Loop;
    }
    case Icode_PROP_INC_DEC : {
        Object lhs = stack[stackTop];
        if (lhs == DBL_MRK) lhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        stack[stackTop] = ScriptRuntime.propIncrDecr(lhs, stringReg,
                                                     cx, iCode[frame.pc]);
        ++frame.pc;
        continue Loop;
    }
    case Token.GETELEM : {
        --stackTop;
        Object lhs = stack[stackTop];
        if (lhs == DBL_MRK) {
            lhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        }
        Object value;
        Object id = stack[stackTop + 1];
        if (id != DBL_MRK) {
            value = ScriptRuntime.getObjectElem(lhs, id, cx, frame.scope);
        } else {
            double d = sDbl[stackTop + 1];
            value = ScriptRuntime.getObjectIndex(lhs, d, cx);
        }
        stack[stackTop] = value;
        continue Loop;
    }
    case Token.SETELEM : {
        stackTop -= 2;
        Object rhs = stack[stackTop + 2];
        if (rhs == DBL_MRK) {
            rhs = ScriptRuntime.wrapNumber(sDbl[stackTop + 2]);
        }
        Object lhs = stack[stackTop];
        if (lhs == DBL_MRK) {
            lhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        }
        Object value;
        Object id = stack[stackTop + 1];
        if (id != DBL_MRK) {
            value = ScriptRuntime.setObjectElem(lhs, id, rhs, cx);
        } else {
            double d = sDbl[stackTop + 1];
            value = ScriptRuntime.setObjectIndex(lhs, d, rhs, cx);
        }
        stack[stackTop] = value;
        continue Loop;
    }
    case Icode_ELEM_INC_DEC: {
        Object rhs = stack[stackTop];
        if (rhs == DBL_MRK) rhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;
        Object lhs = stack[stackTop];
        if (lhs == DBL_MRK) lhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        stack[stackTop] = ScriptRuntime.elemIncrDecr(lhs, rhs, cx,
                                                     iCode[frame.pc]);
        ++frame.pc;
        continue Loop;
    }
    case Token.GET_REF : {
        Ref ref = (Ref)stack[stackTop];
        stack[stackTop] = ScriptRuntime.refGet(ref, cx);
        continue Loop;
    }
    case Token.SET_REF : {
        Object value = stack[stackTop];
        if (value == DBL_MRK) value = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;
        Ref ref = (Ref)stack[stackTop];
        stack[stackTop] = ScriptRuntime.refSet(ref, value, cx);
        continue Loop;
    }
    case Token.DEL_REF : {
        Ref ref = (Ref)stack[stackTop];
        stack[stackTop] = ScriptRuntime.refDel(ref, cx);
        continue Loop;
    }
    case Icode_REF_INC_DEC : {
        Ref ref = (Ref)stack[stackTop];
        stack[stackTop] = ScriptRuntime.refIncrDecr(ref, cx, iCode[frame.pc]);
        ++frame.pc;
        continue Loop;
    }
    case Token.LOCAL_LOAD :
        ++stackTop;
        indexReg += frame.localShift;
        stack[stackTop] = stack[indexReg];
        sDbl[stackTop] = sDbl[indexReg];
        continue Loop;
    case Icode_LOCAL_CLEAR :
        indexReg += frame.localShift;
        stack[indexReg] = null;
        continue Loop;
    case Icode_NAME_AND_THIS :
        // stringReg: name
        ++stackTop;
        stack[stackTop] = ScriptRuntime.getNameFunctionAndThis(stringReg,
                                                               cx, frame.scope);
        ++stackTop;
        stack[stackTop] = ScriptRuntime.lastStoredScriptable(cx);
        continue Loop;
    case Icode_PROP_AND_THIS: {
        Object obj = stack[stackTop];
        if (obj == DBL_MRK) obj = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        // stringReg: property
        stack[stackTop] = ScriptRuntime.getPropFunctionAndThis(obj, stringReg,
                                                               cx, frame.scope);
        ++stackTop;
        stack[stackTop] = ScriptRuntime.lastStoredScriptable(cx);
        continue Loop;
    }
    case Icode_ELEM_AND_THIS: {
        Object obj = stack[stackTop - 1];
        if (obj == DBL_MRK) obj = ScriptRuntime.wrapNumber(sDbl[stackTop - 1]);
        Object id = stack[stackTop];
        if (id == DBL_MRK) id = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        stack[stackTop - 1] = ScriptRuntime.getElemFunctionAndThis(obj, id, cx);
        stack[stackTop] = ScriptRuntime.lastStoredScriptable(cx);
        continue Loop;
    }
    case Icode_VALUE_AND_THIS : {
        Object value = stack[stackTop];
        if (value == DBL_MRK) value = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        stack[stackTop] = ScriptRuntime.getValueFunctionAndThis(value, cx);
        ++stackTop;
        stack[stackTop] = ScriptRuntime.lastStoredScriptable(cx);
        continue Loop;
    }
    case Icode_CALLSPECIAL : {
        if (instructionCounting) {
            cx.instructionCount += INVOCATION_COST;
        }
        int callType = iCode[frame.pc] & 0xFF;
        boolean isNew =  (iCode[frame.pc + 1] != 0);
        int sourceLine = getIndex(iCode, frame.pc + 2);

        // indexReg: number of arguments
        if (isNew) {
            // stack change: function arg0 .. argN -> newResult
            stackTop -= indexReg;

            Object function = stack[stackTop];
            if (function == DBL_MRK)
                function = ScriptRuntime.wrapNumber(sDbl[stackTop]);
            Object[] outArgs = getArgsArray(
                                   stack, sDbl, stackTop + 1, indexReg);
            stack[stackTop] = ScriptRuntime.newSpecial(
                                  cx, function, outArgs, frame.scope, callType);
        } else {
            // stack change: function thisObj arg0 .. argN -> result
            stackTop -= 1 + indexReg;

            // Call code generation ensure that stack here
            // is ... Callable Scriptable
            Scriptable functionThis = (Scriptable)stack[stackTop + 1];
            Callable function = (Callable)stack[stackTop];
            Object[] outArgs = getArgsArray(
                                   stack, sDbl, stackTop + 2, indexReg);
            stack[stackTop] = ScriptRuntime.callSpecial(
                                  cx, function, functionThis, outArgs,
                                  frame.scope, frame.thisObj, callType,
                                  frame.idata.itsSourceFile, sourceLine);
        }
        frame.pc += 4;
        continue Loop;
    }
    case Token.CALL :
    case Icode_TAIL_CALL :
    case Token.REF_CALL : {
        if (instructionCounting) {
            cx.instructionCount += INVOCATION_COST;
        }
        // stack change: function thisObj arg0 .. argN -> result
        // indexReg: number of arguments
        stackTop -= 1 + indexReg;

        // CALL generation ensures that fun and funThisObj
        // are already Scriptable and Callable objects respectively
        Callable fun = (Callable)stack[stackTop];
        Scriptable funThisObj = (Scriptable)stack[stackTop + 1];
        if (op == Token.REF_CALL) {
            Object[] outArgs = getArgsArray(stack, sDbl, stackTop + 2,
                                            indexReg);
            stack[stackTop] = ScriptRuntime.callRef(fun, funThisObj,
                                                    outArgs, cx);
            continue Loop;
        }
        Scriptable calleeScope = frame.scope;
        if (frame.useActivation) {
            calleeScope = ScriptableObject.getTopLevelScope(frame.scope);
        }
        if (fun instanceof InterpretedFunction) {
            InterpretedFunction ifun = (InterpretedFunction)fun;
            if (frame.fnOrScript.securityDomain == ifun.securityDomain) {
                CallFrame callParentFrame = frame;
                CallFrame calleeFrame = new CallFrame();
                if (op == Icode_TAIL_CALL) {
                    // In principle tail call can re-use the current
                    // frame and its stack arrays but it is hard to
                    // do properly. Any exceptions that can legally
                    // happen during frame re-initialization including
                    // StackOverflowException during innocent looking
                    // System.arraycopy may leave the current frame
                    // data corrupted leading to undefined behaviour
                    // in the catch code bellow that unwinds JS stack
                    // on exceptions. Then there is issue about frame release
                    // end exceptions there.
                    // To avoid frame allocation a released frame
                    // can be cached for re-use which would also benefit
                    // non-tail calls but it is not clear that this caching
                    // would gain in performance due to potentially
                    // bad interaction with GC.
                    callParentFrame = frame.parentFrame;
                    // Release the current frame. See Bug #344501 to see why
                    // it is being done here.
                    exitFrame(cx, frame, null);
                }
                initFrame(cx, calleeScope, funThisObj, stack, sDbl,
                          stackTop + 2, indexReg, ifun, callParentFrame,
                          calleeFrame);
                if (op != Icode_TAIL_CALL) {
                    frame.savedStackTop = stackTop;
                    frame.savedCallOp = op;
                }
                frame = calleeFrame;
                continue StateLoop;
            }
        }

        if (fun instanceof NativeContinuation) {
            // Jump to the captured continuation
            ContinuationJump cjump;
            cjump = new ContinuationJump((NativeContinuation)fun, frame);

            // continuation result is the first argument if any
            // of continuation call
            if (indexReg == 0) {
                cjump.result = undefined;
            } else {
                cjump.result = stack[stackTop + 2];
                cjump.resultDbl = sDbl[stackTop + 2];
            }

            // Start the real unwind job
            throwable = cjump;
            break withoutExceptions;
        }

        if (fun instanceof IdFunctionObject) {
            IdFunctionObject ifun = (IdFunctionObject)fun;
            if (NativeContinuation.isContinuationConstructor(ifun)) {
                frame.stack[stackTop] = captureContinuation(cx,
                        frame.parentFrame, false);
                continue Loop;
            }
            // Bug 405654 -- make best effort to keep Function.apply and 
            // Function.call within this interpreter loop invocation
            if (BaseFunction.isApplyOrCall(ifun)) {
                Callable applyCallable = ScriptRuntime.getCallable(funThisObj);
                if (applyCallable instanceof InterpretedFunction) {
                    InterpretedFunction iApplyCallable = (InterpretedFunction)applyCallable;
                    if (frame.fnOrScript.securityDomain == iApplyCallable.securityDomain) {
                        frame = initFrameForApplyOrCall(cx, frame, indexReg,
                                stack, sDbl, stackTop, op, calleeScope, ifun,
                                iApplyCallable);
                        continue StateLoop;
                    }
                }
            }
        }

        // Bug 447697 -- make best effort to keep __noSuchMethod__ within this  
        // interpreter loop invocation
        if (fun instanceof NoSuchMethodShim) {
            // get the shim and the actual method
            NoSuchMethodShim noSuchMethodShim = (NoSuchMethodShim) fun;
            Callable noSuchMethodMethod = noSuchMethodShim.noSuchMethodMethod;
            // if the method is in fact an InterpretedFunction
            if (noSuchMethodMethod instanceof InterpretedFunction) {
                InterpretedFunction ifun = (InterpretedFunction) noSuchMethodMethod;
                if (frame.fnOrScript.securityDomain == ifun.securityDomain) {
                    frame = initFrameForNoSuchMethod(cx, frame, indexReg, stack, sDbl,
                                             stackTop, op, funThisObj, calleeScope,
                                             noSuchMethodShim, ifun);
                    continue StateLoop;
                }
            }
        }

        cx.lastInterpreterFrame = frame;
        frame.savedCallOp = op;
        frame.savedStackTop = stackTop;
        stack[stackTop] = fun.call(cx, calleeScope, funThisObj, 
                getArgsArray(stack, sDbl, stackTop + 2, indexReg));
        cx.lastInterpreterFrame = null;

        continue Loop;
    }
    case Token.NEW : {
        if (instructionCounting) {
            cx.instructionCount += INVOCATION_COST;
        }
        // stack change: function arg0 .. argN -> newResult
        // indexReg: number of arguments
        stackTop -= indexReg;

        Object lhs = stack[stackTop];
        if (lhs instanceof InterpretedFunction) {
            InterpretedFunction f = (InterpretedFunction)lhs;
            if (frame.fnOrScript.securityDomain == f.securityDomain) {
                Scriptable newInstance = f.createObject(cx, frame.scope);
                CallFrame calleeFrame = new CallFrame();
                initFrame(cx, frame.scope, newInstance, stack, sDbl,
                          stackTop + 1, indexReg, f, frame,
                          calleeFrame);

                stack[stackTop] = newInstance;
                frame.savedStackTop = stackTop;
                frame.savedCallOp = op;
                frame = calleeFrame;
                continue StateLoop;
            }
        }
        if (!(lhs instanceof Function)) {
            if (lhs == DBL_MRK) lhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
            throw ScriptRuntime.notFunctionError(lhs);
        }
        Function fun = (Function)lhs;

        if (fun instanceof IdFunctionObject) {
            IdFunctionObject ifun = (IdFunctionObject)fun;
            if (NativeContinuation.isContinuationConstructor(ifun)) {
                frame.stack[stackTop] =
                    captureContinuation(cx, frame.parentFrame, false);
                continue Loop;
            }
        }

        Object[] outArgs = getArgsArray(stack, sDbl, stackTop + 1, indexReg);
        stack[stackTop] = fun.construct(cx, frame.scope, outArgs);
        continue Loop;
    }
    case Token.TYPEOF : {
        Object lhs = stack[stackTop];
        if (lhs == DBL_MRK) lhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        stack[stackTop] = ScriptRuntime.typeof(lhs);
        continue Loop;
    }
    case Icode_TYPEOFNAME :
        stack[++stackTop] = ScriptRuntime.typeofName(frame.scope, stringReg);
        continue Loop;
    case Token.STRING :
        stack[++stackTop] = stringReg;
        continue Loop;
    case Icode_SHORTNUMBER :
        ++stackTop;
        stack[stackTop] = DBL_MRK;
        sDbl[stackTop] = getShort(iCode, frame.pc);
        frame.pc += 2;
        continue Loop;
    case Icode_INTNUMBER :
        ++stackTop;
        stack[stackTop] = DBL_MRK;
        sDbl[stackTop] = getInt(iCode, frame.pc);
        frame.pc += 4;
        continue Loop;
    case Token.NUMBER :
        ++stackTop;
        stack[stackTop] = DBL_MRK;
        sDbl[stackTop] = frame.idata.itsDoubleTable[indexReg];
        continue Loop;
    case Token.NAME :
        stack[++stackTop] = ScriptRuntime.name(cx, frame.scope, stringReg);
        continue Loop;
    case Icode_NAME_INC_DEC :
        stack[++stackTop] = ScriptRuntime.nameIncrDecr(frame.scope, stringReg,
                                                       cx, iCode[frame.pc]);
        ++frame.pc;
        continue Loop;
    case Icode_SETCONSTVAR1:
        indexReg = iCode[frame.pc++];
        // fallthrough
    case Token.SETCONSTVAR :
        if (!frame.useActivation) {
            if ((varAttributes[indexReg] & ScriptableObject.READONLY) == 0) {
                throw Context.reportRuntimeError1("msg.var.redecl",
                                                  frame.idata.argNames[indexReg]);
            }
            if ((varAttributes[indexReg] & ScriptableObject.UNINITIALIZED_CONST)
                != 0)
            {
                vars[indexReg] = stack[stackTop];
                varAttributes[indexReg] &= ~ScriptableObject.UNINITIALIZED_CONST;
                varDbls[indexReg] = sDbl[stackTop];
            }
        } else {
            Object val = stack[stackTop];
            if (val == DBL_MRK) val = ScriptRuntime.wrapNumber(sDbl[stackTop]);
            stringReg = frame.idata.argNames[indexReg];
            if (frame.scope instanceof ConstProperties) {
                ConstProperties cp = (ConstProperties)frame.scope;
                cp.putConst(stringReg, frame.scope, val);
            } else
                throw Kit.codeBug();
        }
        continue Loop;
    case Icode_SETVAR1:
        indexReg = iCode[frame.pc++];
        // fallthrough
    case Token.SETVAR :
        if (!frame.useActivation) {
            if ((varAttributes[indexReg] & ScriptableObject.READONLY) == 0) {
                vars[indexReg] = stack[stackTop];
                varDbls[indexReg] = sDbl[stackTop];
            }
        } else {
            Object val = stack[stackTop];
            if (val == DBL_MRK) val = ScriptRuntime.wrapNumber(sDbl[stackTop]);
            stringReg = frame.idata.argNames[indexReg];
            frame.scope.put(stringReg, frame.scope, val);
        }
        continue Loop;
    case Icode_GETVAR1:
        indexReg = iCode[frame.pc++];
        // fallthrough
    case Token.GETVAR :
        ++stackTop;
        if (!frame.useActivation) {
            stack[stackTop] = vars[indexReg];
            sDbl[stackTop] = varDbls[indexReg];
        } else {
            stringReg = frame.idata.argNames[indexReg];
            stack[stackTop] = frame.scope.get(stringReg, frame.scope);
        }
        continue Loop;
    case Icode_VAR_INC_DEC : {
        // indexReg : varindex
        ++stackTop;
        int incrDecrMask = iCode[frame.pc];
        if (!frame.useActivation) {
            stack[stackTop] = DBL_MRK;
            Object varValue = vars[indexReg];
            double d;
            if (varValue == DBL_MRK) {
                d = varDbls[indexReg];
            } else {
                d = ScriptRuntime.toNumber(varValue);
                vars[indexReg] = DBL_MRK;
            }
            double d2 = ((incrDecrMask & Node.DECR_FLAG) == 0)
                        ? d + 1.0 : d - 1.0;
            varDbls[indexReg] = d2;
            sDbl[stackTop] = ((incrDecrMask & Node.POST_FLAG) == 0) ? d2 : d;
        } else {
            String varName = frame.idata.argNames[indexReg];
            stack[stackTop] = ScriptRuntime.nameIncrDecr(frame.scope, varName,
                                                         cx, incrDecrMask);
        }
        ++frame.pc;
        continue Loop;
    }
    case Icode_ZERO :
        ++stackTop;
        stack[stackTop] = DBL_MRK;
        sDbl[stackTop] = 0;
        continue Loop;
    case Icode_ONE :
        ++stackTop;
        stack[stackTop] = DBL_MRK;
        sDbl[stackTop] = 1;
        continue Loop;
    case Token.NULL :
        stack[++stackTop] = null;
        continue Loop;
    case Token.THIS :
        stack[++stackTop] = frame.thisObj;
        continue Loop;
    case Token.THISFN :
        stack[++stackTop] = frame.fnOrScript;
        continue Loop;
    case Token.FALSE :
        stack[++stackTop] = Boolean.FALSE;
        continue Loop;
    case Token.TRUE :
        stack[++stackTop] = Boolean.TRUE;
        continue Loop;
    case Icode_UNDEF :
        stack[++stackTop] = undefined;
        continue Loop;
    case Token.ENTERWITH : {
        Object lhs = stack[stackTop];
        if (lhs == DBL_MRK) lhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;
        frame.scope = ScriptRuntime.enterWith(lhs, cx, frame.scope);
        continue Loop;
    }
    case Token.LEAVEWITH :
        frame.scope = ScriptRuntime.leaveWith(frame.scope);
        continue Loop;
    case Token.CATCH_SCOPE : {
        // stack top: exception object
        // stringReg: name of exception variable
        // indexReg: local for exception scope
        --stackTop;
        indexReg += frame.localShift;

        boolean afterFirstScope =  (frame.idata.itsICode[frame.pc] != 0);
        Throwable caughtException = (Throwable)stack[stackTop + 1];
        Scriptable lastCatchScope;
        if (!afterFirstScope) {
            lastCatchScope = null;
        } else {
            lastCatchScope = (Scriptable)stack[indexReg];
        }
        stack[indexReg] = ScriptRuntime.newCatchScope(caughtException,
                                                      lastCatchScope, stringReg,
                                                      cx, frame.scope);
        ++frame.pc;
        continue Loop;
    }
    case Token.ENUM_INIT_KEYS :
    case Token.ENUM_INIT_VALUES :
    case Token.ENUM_INIT_ARRAY : {
        Object lhs = stack[stackTop];
        if (lhs == DBL_MRK) lhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;
        indexReg += frame.localShift;
        int enumType = op == Token.ENUM_INIT_KEYS 
                         ? ScriptRuntime.ENUMERATE_KEYS :
                       op == Token.ENUM_INIT_VALUES 
                         ? ScriptRuntime.ENUMERATE_VALUES :
                       ScriptRuntime.ENUMERATE_ARRAY;
        stack[indexReg] = ScriptRuntime.enumInit(lhs, cx, enumType);
        continue Loop;
    }
    case Token.ENUM_NEXT :
    case Token.ENUM_ID : {
        indexReg += frame.localShift;
        Object val = stack[indexReg];
        ++stackTop;
        stack[stackTop] = (op == Token.ENUM_NEXT)
                          ? (Object)ScriptRuntime.enumNext(val)
                          : (Object)ScriptRuntime.enumId(val, cx);
        continue Loop;
    }
    case Token.REF_SPECIAL : {
        //stringReg: name of special property
        Object obj = stack[stackTop];
        if (obj == DBL_MRK) obj = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        stack[stackTop] = ScriptRuntime.specialRef(obj, stringReg, cx);
        continue Loop;
    }
    case Token.REF_MEMBER: {
        //indexReg: flags
        Object elem = stack[stackTop];
        if (elem == DBL_MRK) elem = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;
        Object obj = stack[stackTop];
        if (obj == DBL_MRK) obj = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        stack[stackTop] = ScriptRuntime.memberRef(obj, elem, cx, indexReg);
        continue Loop;
    }
    case Token.REF_NS_MEMBER: {
        //indexReg: flags
        Object elem = stack[stackTop];
        if (elem == DBL_MRK) elem = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;
        Object ns = stack[stackTop];
        if (ns == DBL_MRK) ns = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;
        Object obj = stack[stackTop];
        if (obj == DBL_MRK) obj = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        stack[stackTop] = ScriptRuntime.memberRef(obj, ns, elem, cx, indexReg);
        continue Loop;
    }
    case Token.REF_NAME: {
        //indexReg: flags
        Object name = stack[stackTop];
        if (name == DBL_MRK) name = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        stack[stackTop] = ScriptRuntime.nameRef(name, cx, frame.scope,
                                                indexReg);
        continue Loop;
    }
    case Token.REF_NS_NAME: {
        //indexReg: flags
        Object name = stack[stackTop];
        if (name == DBL_MRK) name = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;
        Object ns = stack[stackTop];
        if (ns == DBL_MRK) ns = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        stack[stackTop] = ScriptRuntime.nameRef(ns, name, cx, frame.scope,
                                                indexReg);
        continue Loop;
    }
    case Icode_SCOPE_LOAD :
        indexReg += frame.localShift;
        frame.scope = (Scriptable)stack[indexReg];
        continue Loop;
    case Icode_SCOPE_SAVE :
        indexReg += frame.localShift;
        stack[indexReg] = frame.scope;
        continue Loop;
    case Icode_CLOSURE_EXPR :
        stack[++stackTop] = InterpretedFunction.createFunction(cx, frame.scope,
                                                               frame.fnOrScript,
                                                               indexReg);
        continue Loop;
    case Icode_CLOSURE_STMT :
        initFunction(cx, frame.scope, frame.fnOrScript, indexReg);
        continue Loop;
    case Token.REGEXP :
        stack[++stackTop] = frame.scriptRegExps[indexReg];
        continue Loop;
    case Icode_LITERAL_NEW :
        // indexReg: number of values in the literal
        ++stackTop;
        stack[stackTop] = new int[indexReg];
        ++stackTop;
        stack[stackTop] = new Object[indexReg];
        sDbl[stackTop] = 0;
        continue Loop;
    case Icode_LITERAL_SET : {
        Object value = stack[stackTop];
        if (value == DBL_MRK) value = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;
        int i = (int)sDbl[stackTop];
        ((Object[])stack[stackTop])[i] = value;
        sDbl[stackTop] = i + 1;
        continue Loop;
    }
    case Icode_LITERAL_GETTER : {
        Object value = stack[stackTop];
        --stackTop;
        int i = (int)sDbl[stackTop];
        ((Object[])stack[stackTop])[i] = value;
        ((int[])stack[stackTop - 1])[i] = -1;
        sDbl[stackTop] = i + 1;
        continue Loop;
    }
    case Icode_LITERAL_SETTER : {
        Object value = stack[stackTop];
        --stackTop;
        int i = (int)sDbl[stackTop];
        ((Object[])stack[stackTop])[i] = value;
        ((int[])stack[stackTop - 1])[i] = +1;
        sDbl[stackTop] = i + 1;
        continue Loop;
    }
    case Token.ARRAYLIT :
    case Icode_SPARE_ARRAYLIT :
    case Token.OBJECTLIT : {
        Object[] data = (Object[])stack[stackTop];
        --stackTop;
        int[] getterSetters = (int[])stack[stackTop];
        Object val;
        if (op == Token.OBJECTLIT) {
            Object[] ids = (Object[])frame.idata.literalIds[indexReg];
            val = ScriptRuntime.newObjectLiteral(ids, data, getterSetters, cx,
                    frame.scope);
        } else {
            int[] skipIndexces = null;
            if (op == Icode_SPARE_ARRAYLIT) {
                skipIndexces = (int[])frame.idata.literalIds[indexReg];
            }
            val = ScriptRuntime.newArrayLiteral(data, skipIndexces, cx,
                                                frame.scope);
        }
        stack[stackTop] = val;
        continue Loop;
    }
    case Icode_ENTERDQ : {
        Object lhs = stack[stackTop];
        if (lhs == DBL_MRK) lhs = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        --stackTop;
        frame.scope = ScriptRuntime.enterDotQuery(lhs, frame.scope);
        continue Loop;
    }
    case Icode_LEAVEDQ : {
        boolean valBln = stack_boolean(frame, stackTop);
        Object x = ScriptRuntime.updateDotQuery(valBln, frame.scope);
        if (x != null) {
            stack[stackTop] = x;
            frame.scope = ScriptRuntime.leaveDotQuery(frame.scope);
            frame.pc += 2;
            continue Loop;
        }
        // reset stack and PC to code after ENTERDQ
        --stackTop;
        break jumplessRun;
    }
    case Token.DEFAULTNAMESPACE : {
        Object value = stack[stackTop];
        if (value == DBL_MRK) value = ScriptRuntime.wrapNumber(sDbl[stackTop]);
        stack[stackTop] = ScriptRuntime.setDefaultNamespace(value, cx);
        continue Loop;
    }
    case Token.ESCXMLATTR : {
        Object value = stack[stackTop];
        if (value != DBL_MRK) {
            stack[stackTop] = ScriptRuntime.escapeAttributeValue(value, cx);
        }
        continue Loop;
    }
    case Token.ESCXMLTEXT : {
        Object value = stack[stackTop];
        if (value != DBL_MRK) {
            stack[stackTop] = ScriptRuntime.escapeTextValue(value, cx);
        }
        continue Loop;
    }
    case Icode_DEBUGGER:
        if (frame.debuggerFrame != null) {
            frame.debuggerFrame.onDebuggerStatement(cx);
        }
        continue Loop;
    case Icode_LINE :
        frame.pcSourceLineStart = frame.pc;
        if (frame.debuggerFrame != null) {
            int line = getIndex(iCode, frame.pc);
            frame.debuggerFrame.onLineChange(cx, line);
        }
        frame.pc += 2;
        continue Loop;
    case Icode_REG_IND_C0:
        indexReg = 0;
        continue Loop;
    case Icode_REG_IND_C1:
        indexReg = 1;
        continue Loop;
    case Icode_REG_IND_C2:
        indexReg = 2;
        continue Loop;
    case Icode_REG_IND_C3:
        indexReg = 3;
        continue Loop;
    case Icode_REG_IND_C4:
        indexReg = 4;
        continue Loop;
    case Icode_REG_IND_C5:
        indexReg = 5;
        continue Loop;
    case Icode_REG_IND1:
        indexReg = 0xFF & iCode[frame.pc];
        ++frame.pc;
        continue Loop;
    case Icode_REG_IND2:
        indexReg = getIndex(iCode, frame.pc);
        frame.pc += 2;
        continue Loop;
    case Icode_REG_IND4:
        indexReg = getInt(iCode, frame.pc);
        frame.pc += 4;
        continue Loop;
    case Icode_REG_STR_C0:
        stringReg = strings[0];
        continue Loop;
    case Icode_REG_STR_C1:
        stringReg = strings[1];
        continue Loop;
    case Icode_REG_STR_C2:
        stringReg = strings[2];
        continue Loop;
    case Icode_REG_STR_C3:
        stringReg = strings[3];
        continue Loop;
    case Icode_REG_STR1:
        stringReg = strings[0xFF & iCode[frame.pc]];
        ++frame.pc;
        continue Loop;
    case Icode_REG_STR2:
        stringReg = strings[getIndex(iCode, frame.pc)];
        frame.pc += 2;
        continue Loop;
    case Icode_REG_STR4:
        stringReg = strings[getInt(iCode, frame.pc)];
        frame.pc += 4;
        continue Loop;
    default :
        dumpICode(frame.idata);
        throw new RuntimeException(
            "Unknown icode : "+op+" @ pc : "+(frame.pc-1));
}  // end of interpreter switch

                    } // end of jumplessRun label block

                    // This should be reachable only for jump implementation
                    // when pc points to encoded target offset
                    if (instructionCounting) {
                        addInstructionCount(cx, frame, 2);
                    }
                    int offset = getShort(iCode, frame.pc);
                    if (offset != 0) {
                        // -1 accounts for pc pointing to jump opcode + 1
                        frame.pc += offset - 1;
                    } else {
                        frame.pc = frame.idata.longJumps.
                                       getExistingInt(frame.pc);
                    }
                    if (instructionCounting) {
                        frame.pcPrevBranch = frame.pc;
                    }
                    continue Loop;

                } // end of Loop: for

                exitFrame(cx, frame, null);
                interpreterResult = frame.result;
                interpreterResultDbl = frame.resultDbl;
                if (frame.parentFrame != null) {
                    frame = frame.parentFrame;
                    if (frame.frozen) {
                        frame = frame.cloneFrozen();
                    }
                    setCallResult(
                        frame, interpreterResult, interpreterResultDbl);
                    interpreterResult = null; // Help GC
                    continue StateLoop;
                }
                break StateLoop;

            }  // end of interpreter withoutExceptions: try
            catch (Throwable ex) {
                if (throwable != null) {
                    // This is serious bug and it is better to track it ASAP
                    ex.printStackTrace(System.err);
                    throw new IllegalStateException();
                }
                throwable = ex;
            }

            // This should be reachable only after above catch or from
            // finally when it needs to propagate exception or from
            // explicit throw
            if (throwable == null) Kit.codeBug();

            // Exception type
            final int EX_CATCH_STATE = 2; // Can execute JS catch
            final int EX_FINALLY_STATE = 1; // Can execute JS finally
            final int EX_NO_JS_STATE = 0; // Terminate JS execution

            int exState;
            ContinuationJump cjump = null;

            if (generatorState != null &&
                generatorState.operation == NativeGenerator.GENERATOR_CLOSE &&
                throwable == generatorState.value)
            {
                exState = EX_FINALLY_STATE;            	
            } else if (throwable instanceof JavaScriptException) {
                exState = EX_CATCH_STATE;
            } else if (throwable instanceof EcmaError) {
                // an offical ECMA error object,
                exState = EX_CATCH_STATE;
            } else if (throwable instanceof EvaluatorException) {
                exState = EX_CATCH_STATE;
            } else if (throwable instanceof RuntimeException) {
                exState = cx.hasFeature(Context.FEATURE_ENHANCED_JAVA_ACCESS)
                          ? EX_CATCH_STATE
                          : EX_FINALLY_STATE;
            } else if (throwable instanceof Error) {
                exState = cx.hasFeature(Context.FEATURE_ENHANCED_JAVA_ACCESS)
                          ? EX_CATCH_STATE
                          : EX_NO_JS_STATE;
            } else if (throwable instanceof ContinuationJump) {
                // It must be ContinuationJump
                exState = EX_FINALLY_STATE;
                cjump = (ContinuationJump)throwable;
            } else {
                exState = cx.hasFeature(Context.FEATURE_ENHANCED_JAVA_ACCESS)
                          ? EX_CATCH_STATE
                          : EX_FINALLY_STATE;
            }

            if (instructionCounting) {
                try {
                    addInstructionCount(cx, frame, EXCEPTION_COST);
                } catch (RuntimeException ex) {
                    throwable = ex;
                    exState = EX_FINALLY_STATE;
                } catch (Error ex) {
                    // Error from instruction counting
                    //     => unconditionally terminate JS
                    throwable = ex;
                    cjump = null;
                    exState = EX_NO_JS_STATE;
                }
            }
            if (frame.debuggerFrame != null
                && throwable instanceof RuntimeException)
            {
                // Call debugger only for RuntimeException
                RuntimeException rex = (RuntimeException)throwable;
                try {
                    frame.debuggerFrame.onExceptionThrown(cx, rex);
                } catch (Throwable ex) {
                    // Any exception from debugger
                    //     => unconditionally terminate JS
                    throwable = ex;
                    cjump = null;
                    exState = EX_NO_JS_STATE;
                }
            }

            for (;;) {
                if (exState != EX_NO_JS_STATE) {
                    boolean onlyFinally = (exState != EX_CATCH_STATE);
                    indexReg = getExceptionHandler(frame, onlyFinally);
                    if (indexReg >= 0) {
                        // We caught an exception, restart the loop
                        // with exception pending the processing at the loop
                        // start
                        continue StateLoop;
                    }
                }
                // No allowed exception handlers in this frame, unwind
                // to parent and try to look there

                exitFrame(cx, frame, throwable);

                frame = frame.parentFrame;
                if (frame == null) { break; }
                if (cjump != null && cjump.branchFrame == frame) {
                    // Continuation branch point was hit,
                    // restart the state loop to reenter continuation
                    indexReg = -1;
                    continue StateLoop;
                }
            }

            // No more frames, rethrow the exception or deal with continuation
            if (cjump != null) {
                if (cjump.branchFrame != null) {
                    // The above loop should locate the top frame
                    Kit.codeBug();
                }
                if (cjump.capturedFrame != null) {
                    // Restarting detached continuation
                    indexReg = -1;
                    continue StateLoop;
                }
                // Return continuation result to the caller
                interpreterResult = cjump.result;
                interpreterResultDbl = cjump.resultDbl;
                throwable = null;
            }
            break StateLoop;

        } // end of StateLoop: for(;;)

        // Do cleanups/restorations before the final return or throw

        if (cx.previousInterpreterInvocations != null
            && cx.previousInterpreterInvocations.size() != 0)
        {
            cx.lastInterpreterFrame
                = cx.previousInterpreterInvocations.pop();
        } else {
            // It was the last interpreter frame on the stack
            cx.lastInterpreterFrame = null;
            // Force GC of the value cx.previousInterpreterInvocations
            cx.previousInterpreterInvocations = null;
        }

        if (throwable != null) {
            if (throwable instanceof RuntimeException) {
                throw (RuntimeException)throwable;
            } else {
                // Must be instance of Error or code bug
                throw (Error)throwable;
            }
        }

        return (interpreterResult != DBL_MRK)
               ? interpreterResult
               : ScriptRuntime.wrapNumber(interpreterResultDbl);
    }

    /**
     * Call __noSuchMethod__.
     */
    private static CallFrame initFrameForNoSuchMethod(Context cx,
            CallFrame frame, int indexReg, Object[] stack, double[] sDbl,
            int stackTop, int op, Scriptable funThisObj, Scriptable calleeScope,
            NoSuchMethodShim noSuchMethodShim, InterpretedFunction ifun)
    {
        // create an args array from the stack
        Object[] argsArray = null;
        // exactly like getArgsArray except that the first argument
        // is the method name from the shim
        int shift = stackTop + 2;
        Object[] elements = new Object[indexReg];
        for (int i=0; i < indexReg; ++i, ++shift) {
            Object val = stack[shift];
            if (val == UniqueTag.DOUBLE_MARK) {
                val = ScriptRuntime.wrapNumber(sDbl[shift]);
            }
            elements[i] = val;
        }
        argsArray = new Object[2];
        argsArray[0] = noSuchMethodShim.methodName;
        argsArray[1] = cx.newArray(calleeScope, elements);
        
        // exactly the same as if it's a regular InterpretedFunction
        CallFrame callParentFrame = frame;
        CallFrame calleeFrame = new CallFrame();
        if (op == Icode_TAIL_CALL) {
            callParentFrame = frame.parentFrame;
            exitFrame(cx, frame, null);
        }
        // init the frame with the underlying method with the 
        // adjusted args array and shim's function
        initFrame(cx, calleeScope, funThisObj, argsArray, null,
          0, 2, ifun, callParentFrame, calleeFrame);
        if (op != Icode_TAIL_CALL) {
            frame.savedStackTop = stackTop;
            frame.savedCallOp = op;
        }
        return calleeFrame;
    }
    
    private static boolean shallowEquals(Object[] stack, double[] sDbl,
            int stackTop)
    {
        Object rhs = stack[stackTop + 1];
        Object lhs = stack[stackTop];
        final Object DBL_MRK = UniqueTag.DOUBLE_MARK;
        double rdbl, ldbl;
        if (rhs == DBL_MRK) {
            rdbl = sDbl[stackTop + 1];
            if (lhs == DBL_MRK) {
                ldbl = sDbl[stackTop];
            } else if (lhs instanceof Number) {
                ldbl = ((Number)lhs).doubleValue();
            } else {
                return false;
            }
        } else if (lhs == DBL_MRK) {
            ldbl = sDbl[stackTop];
            if (rhs == DBL_MRK) {
                rdbl = sDbl[stackTop + 1];
            } else if (rhs instanceof Number) {
                rdbl = ((Number)rhs).doubleValue();
            } else {
                return false;
            }
        } else {
            return ScriptRuntime.shallowEq(lhs, rhs);
        }
        return (ldbl == rdbl);
    }

    private static CallFrame processThrowable(Context cx, Object throwable,
                                              CallFrame frame, int indexReg,
                                              boolean instructionCounting)
    {
        // Recovering from exception, indexReg contains
        // the index of handler

        if (indexReg >= 0) {
            // Normal exception handler, transfer
            // control appropriately

            if (frame.frozen) {
                // XXX Deal with exceptios!!!
                frame = frame.cloneFrozen();
            }
            
            int[] table = frame.idata.itsExceptionTable;

            frame.pc = table[indexReg + EXCEPTION_HANDLER_SLOT];
            if (instructionCounting) {
                frame.pcPrevBranch = frame.pc;
            }

            frame.savedStackTop = frame.emptyStackTop;
            int scopeLocal = frame.localShift
                             + table[indexReg
                                     + EXCEPTION_SCOPE_SLOT];
            int exLocal = frame.localShift
                             + table[indexReg
                                     + EXCEPTION_LOCAL_SLOT];
            frame.scope = (Scriptable)frame.stack[scopeLocal];
            frame.stack[exLocal] = throwable;

            throwable = null;
        } else {
            // Continuation restoration
            ContinuationJump cjump = (ContinuationJump)throwable;

            // Clear throwable to indicate that exceptions are OK
            throwable = null;

            if (cjump.branchFrame != frame) Kit.codeBug();

            // Check that we have at least one frozen frame
            // in the case of detached continuation restoration:
            // unwind code ensure that
            if (cjump.capturedFrame == null) Kit.codeBug();

            // Need to rewind branchFrame, capturedFrame
            // and all frames in between
            int rewindCount = cjump.capturedFrame.frameIndex + 1;
            if (cjump.branchFrame != null) {
                rewindCount -= cjump.branchFrame.frameIndex;
            }

            int enterCount = 0;
            CallFrame[] enterFrames = null;

            CallFrame x = cjump.capturedFrame;
            for (int i = 0; i != rewindCount; ++i) {
                if (!x.frozen) Kit.codeBug();
                if (isFrameEnterExitRequired(x)) {
                    if (enterFrames == null) {
                        // Allocate enough space to store the rest
                        // of rewind frames in case all of them
                        // would require to enter
                        enterFrames = new CallFrame[rewindCount
                                                    - i];
                    }
                    enterFrames[enterCount] = x;
                    ++enterCount;
                }
                x = x.parentFrame;
            }

            while (enterCount != 0) {
                // execute enter: walk enterFrames in the reverse
                // order since they were stored starting from
                // the capturedFrame, not branchFrame
                --enterCount;
                x = enterFrames[enterCount];
                enterFrame(cx, x, ScriptRuntime.emptyArgs, true);
            }

            // Continuation jump is almost done: capturedFrame
            // points to the call to the function that captured
            // continuation, so clone capturedFrame and
            // emulate return that function with the suplied result
            frame = cjump.capturedFrame.cloneFrozen();
            setCallResult(frame, cjump.result, cjump.resultDbl);
            // restart the execution
        }
        frame.throwable = throwable;
        return frame;
    }

    private static Object freezeGenerator(Context cx, CallFrame frame,
                                          int stackTop,
                                          GeneratorState generatorState)
    {
          if (generatorState.operation == NativeGenerator.GENERATOR_CLOSE) {
              // Error: no yields when generator is closing
              throw ScriptRuntime.typeError0("msg.yield.closing");
          }
          // return to our caller (which should be a method of NativeGenerator)
          frame.frozen = true;
          frame.result = frame.stack[stackTop];
          frame.resultDbl = frame.sDbl[stackTop];
          frame.savedStackTop = stackTop;
          frame.pc--; // we want to come back here when we resume
          ScriptRuntime.exitActivationFunction(cx);
          return (frame.result != UniqueTag.DOUBLE_MARK)
              ? frame.result
              : ScriptRuntime.wrapNumber(frame.resultDbl);
    }

    private static Object thawGenerator(CallFrame frame, int stackTop,
                                        GeneratorState generatorState, int op)
    {
          // we are resuming execution
          frame.frozen = false;
          int sourceLine = getIndex(frame.idata.itsICode, frame.pc);
          frame.pc += 2; // skip line number data
          if (generatorState.operation == NativeGenerator.GENERATOR_THROW) {
              // processing a call to <generator>.throw(exception): must
              // act as if exception was thrown from resumption point
              return new JavaScriptException(generatorState.value,
                                                  frame.idata.itsSourceFile,
                                                  sourceLine);
          }
          if (generatorState.operation == NativeGenerator.GENERATOR_CLOSE) {
              return generatorState.value;
          }
          if (generatorState.operation != NativeGenerator.GENERATOR_SEND)
              throw Kit.codeBug();
          if (op == Token.YIELD)
              frame.stack[stackTop] = generatorState.value;
          return Scriptable.NOT_FOUND;
    }

    private static CallFrame initFrameForApplyOrCall(Context cx, CallFrame frame,
            int indexReg, Object[] stack, double[] sDbl, int stackTop, int op,
            Scriptable calleeScope, IdFunctionObject ifun,
            InterpretedFunction iApplyCallable)
    {
        Scriptable applyThis;
        if (indexReg != 0) {
            Object obj = stack[stackTop + 2];
            if (obj == UniqueTag.DOUBLE_MARK)
                obj = ScriptRuntime.wrapNumber(sDbl[stackTop + 2]);
            applyThis = ScriptRuntime.toObjectOrNull(cx, obj);
        }
        else {
            applyThis = null;
        }
        if (applyThis == null) {
            // This covers the case of args[0] == (null|undefined) as well.
            applyThis = ScriptRuntime.getTopCallScope(cx);
        }
        if(op == Icode_TAIL_CALL) {
            exitFrame(cx, frame, null);
            frame = frame.parentFrame;
        }
        else {
            frame.savedStackTop = stackTop;
            frame.savedCallOp = op;
        }
        CallFrame calleeFrame = new CallFrame();
        if(BaseFunction.isApply(ifun)) {
            Object[] callArgs = indexReg < 2 ? ScriptRuntime.emptyArgs : 
                ScriptRuntime.getApplyArguments(cx, stack[stackTop + 3]);
            initFrame(cx, calleeScope, applyThis, callArgs, null, 0, 
                    callArgs.length, iApplyCallable, frame, calleeFrame);
        }
        else {
            // Shift args left
            for(int i = 1; i < indexReg; ++i) {
                stack[stackTop + 1 + i] = stack[stackTop + 2 + i];
                sDbl[stackTop + 1 + i] = sDbl[stackTop + 2 + i];
            }
            int argCount = indexReg < 2 ? 0 : indexReg - 1;
            initFrame(cx, calleeScope, applyThis, stack, sDbl, stackTop + 2, 
                    argCount, iApplyCallable, frame, calleeFrame);
        }
        
        frame = calleeFrame;
        return frame;
    }

    private static void initFrame(Context cx, Scriptable callerScope,
                                  Scriptable thisObj,
                                  Object[] args, double[] argsDbl,
                                  int argShift, int argCount,
                                  InterpretedFunction fnOrScript,
                                  CallFrame parentFrame, CallFrame frame)
    {
        InterpreterData idata = fnOrScript.idata;

        boolean useActivation = idata.itsNeedsActivation;
        DebugFrame debuggerFrame = null;
        if (cx.debugger != null) {
            debuggerFrame = cx.debugger.getFrame(cx, idata);
            if (debuggerFrame != null) {
                useActivation = true;
            }
        }

        if (useActivation) {
            // Copy args to new array to pass to enterActivationFunction
            // or debuggerFrame.onEnter
            if (argsDbl != null) {
                args = getArgsArray(args, argsDbl, argShift, argCount);
            }
            argShift = 0;
            argsDbl = null;
        }

        Scriptable scope;
        if (idata.itsFunctionType != 0) {
            if (!idata.useDynamicScope) {
                scope = fnOrScript.getParentScope();
            } else {
                scope = callerScope;
            }

            if (useActivation) {
                scope = ScriptRuntime.createFunctionActivation(
                            fnOrScript, scope, args);
            }
        } else {
            scope = callerScope;
            ScriptRuntime.initScript(fnOrScript, thisObj, cx, scope,
                                     fnOrScript.idata.evalScriptFlag);
        }

        if (idata.itsNestedFunctions != null) {
            if (idata.itsFunctionType != 0 && !idata.itsNeedsActivation)
                Kit.codeBug();
            for (int i = 0; i < idata.itsNestedFunctions.length; i++) {
                InterpreterData fdata = idata.itsNestedFunctions[i];
                if (fdata.itsFunctionType == FunctionNode.FUNCTION_STATEMENT) {
                    initFunction(cx, scope, fnOrScript, i);
                }
            }
        }

        Scriptable[] scriptRegExps = null;
        if (idata.itsRegExpLiterals != null) {
            // Wrapped regexps for functions are stored in
            // InterpretedFunction
            // but for script which should not contain references to scope
            // the regexps re-wrapped during each script execution
            if (idata.itsFunctionType != 0) {
                scriptRegExps = fnOrScript.functionRegExps;
            } else {
                scriptRegExps = fnOrScript.createRegExpWraps(cx, scope);
            }
        }

        // Initialize args, vars, locals and stack

        int emptyStackTop = idata.itsMaxVars + idata.itsMaxLocals - 1;
        int maxFrameArray = idata.itsMaxFrameArray;
        if (maxFrameArray != emptyStackTop + idata.itsMaxStack + 1)
            Kit.codeBug();

        Object[] stack;
        int[] stackAttributes;
        double[] sDbl;
        boolean stackReuse;
        if (frame.stack != null && maxFrameArray <= frame.stack.length) {
            // Reuse stacks from old frame
            stackReuse = true;
            stack = frame.stack;
            stackAttributes = frame.stackAttributes;
            sDbl = frame.sDbl;
        } else {
            stackReuse = false;
            stack = new Object[maxFrameArray];
            stackAttributes = new int[maxFrameArray];
            sDbl = new double[maxFrameArray];
        }

        int varCount = idata.getParamAndVarCount();
        for (int i = 0; i < varCount; i++) {
            if (idata.getParamOrVarConst(i))
                stackAttributes[i] = ScriptableObject.CONST;
        }
        int definedArgs = idata.argCount;
        if (definedArgs > argCount) { definedArgs = argCount; }

        // Fill the frame structure

        frame.parentFrame = parentFrame;
        frame.frameIndex = (parentFrame == null)
                           ? 0 : parentFrame.frameIndex + 1;
        if(frame.frameIndex > cx.getMaximumInterpreterStackDepth())
        {
            throw Context.reportRuntimeError("Exceeded maximum stack depth");
        }
        frame.frozen = false;

        frame.fnOrScript = fnOrScript;
        frame.idata = idata;

        frame.stack = stack;
        frame.stackAttributes = stackAttributes;
        frame.sDbl = sDbl;
        frame.varSource = frame;
        frame.localShift = idata.itsMaxVars;
        frame.emptyStackTop = emptyStackTop;

        frame.debuggerFrame = debuggerFrame;
        frame.useActivation = useActivation;

        frame.thisObj = thisObj;
        frame.scriptRegExps = scriptRegExps;

        // Initialize initial values of variables that change during
        // interpretation.
        frame.result = Undefined.instance;
        frame.pc = 0;
        frame.pcPrevBranch = 0;
        frame.pcSourceLineStart = idata.firstLinePC;
        frame.scope = scope;

        frame.savedStackTop = emptyStackTop;
        frame.savedCallOp = 0;

        System.arraycopy(args, argShift, stack, 0, definedArgs);
        if (argsDbl != null) {
            System.arraycopy(argsDbl, argShift, sDbl, 0, definedArgs);
        }
        for (int i = definedArgs; i != idata.itsMaxVars; ++i) {
            stack[i] = Undefined.instance;
        }
        if (stackReuse) {
            // Clean the stack part and space beyond stack if any
            // of the old array to allow to GC objects there
            for (int i = emptyStackTop + 1; i != stack.length; ++i) {
                stack[i] = null;
            }
        }

        enterFrame(cx, frame, args, false);
    }

    private static boolean isFrameEnterExitRequired(CallFrame frame)
    {
        return frame.debuggerFrame != null || frame.idata.itsNeedsActivation;
    }

    private static void enterFrame(Context cx, CallFrame frame, Object[] args, 
                                   boolean continuationRestart)
    {
        boolean usesActivation = frame.idata.itsNeedsActivation; 
        boolean isDebugged = frame.debuggerFrame != null;
        if(usesActivation || isDebugged) {
            Scriptable scope = frame.scope;
            if(scope == null) {
                Kit.codeBug();
            } else if (continuationRestart) {
                // Walk the parent chain of frame.scope until a NativeCall is 
                // found. Normally, frame.scope is a NativeCall when called 
                // from initFrame() for a debugged or activatable function. 
                // However, when called from interpretLoop() as part of
                // restarting a continuation, it can also be a NativeWith if 
                // the continuation was captured within a "with" or "catch" 
                // block ("catch" implicitly uses NativeWith to create a scope 
                // to expose the exception variable).
                for(;;) {
                    if(scope instanceof NativeWith) {
                        scope = scope.getParentScope();
                        if (scope == null || (frame.parentFrame != null && 
                                              frame.parentFrame.scope == scope))
                        {
                            // If we get here, we didn't find a NativeCall in 
                            // the call chain before reaching parent frame's 
                            // scope. This should not be possible.
                            Kit.codeBug();
                            break; // Never reached, but keeps the static analyzer happy about "scope" not being null 5 lines above.
                        }
                    }
                    else {
                        break;
                    }
                }
            }
            if (isDebugged) {
                frame.debuggerFrame.onEnter(cx, scope, frame.thisObj, args);
            }
            // Enter activation only when itsNeedsActivation true, 
            // since debugger should not interfere with activation 
            // chaining
            if (usesActivation) {
                ScriptRuntime.enterActivationFunction(cx, scope);
            }
        }
    }

    private static void exitFrame(Context cx, CallFrame frame,
                                  Object throwable)
    {
        if (frame.idata.itsNeedsActivation) {
            ScriptRuntime.exitActivationFunction(cx);
        }

        if (frame.debuggerFrame != null) {
            try {
                if (throwable instanceof Throwable) {
                    frame.debuggerFrame.onExit(cx, true, throwable);
                } else {
                    Object result;
                    ContinuationJump cjump = (ContinuationJump)throwable;
                    if (cjump == null) {
                        result = frame.result;
                    } else {
                        result = cjump.result;
                    }
                    if (result == UniqueTag.DOUBLE_MARK) {
                        double resultDbl;
                        if (cjump == null) {
                            resultDbl = frame.resultDbl;
                        } else {
                            resultDbl = cjump.resultDbl;
                        }
                        result = ScriptRuntime.wrapNumber(resultDbl);
                    }
                    frame.debuggerFrame.onExit(cx, false, result);
                }
            } catch (Throwable ex) {
                System.err.println(
"RHINO USAGE WARNING: onExit terminated with exception");
                ex.printStackTrace(System.err);
            }
        }
    }

    private static void setCallResult(CallFrame frame,
                                      Object callResult,
                                      double callResultDbl)
    {
        if (frame.savedCallOp == Token.CALL) {
            frame.stack[frame.savedStackTop] = callResult;
            frame.sDbl[frame.savedStackTop] = callResultDbl;
        } else if (frame.savedCallOp == Token.NEW) {
            // If construct returns scriptable,
            // then it replaces on stack top saved original instance
            // of the object.
            if (callResult instanceof Scriptable) {
                frame.stack[frame.savedStackTop] = callResult;
            }
        } else {
            Kit.codeBug();
        }
        frame.savedCallOp = 0;
    }
    
    public static NativeContinuation captureContinuation(Context cx) {
        if (cx.lastInterpreterFrame == null ||
            !(cx.lastInterpreterFrame instanceof CallFrame))
        {
            throw new IllegalStateException("Interpreter frames not found");
        }
        return captureContinuation(cx, (CallFrame)cx.lastInterpreterFrame, true);
    }

    private static NativeContinuation captureContinuation(Context cx, CallFrame frame,
        boolean requireContinuationsTopFrame)
    {
        NativeContinuation c = new NativeContinuation();
        ScriptRuntime.setObjectProtoAndParent(
            c, ScriptRuntime.getTopCallScope(cx));

        // Make sure that all frames are frozen
        CallFrame x = frame;
        CallFrame outermost = frame;
        while (x != null && !x.frozen) {
            x.frozen = true;
            // Allow to GC unused stack space
            for (int i = x.savedStackTop + 1; i != x.stack.length; ++i) {
                // Allow to GC unused stack space
                x.stack[i] = null;
                x.stackAttributes[i] = ScriptableObject.EMPTY;
            }
            if (x.savedCallOp == Token.CALL) {
                // the call will always overwrite the stack top with the result
                x.stack[x.savedStackTop] = null;
            } else {
                if (x.savedCallOp != Token.NEW) Kit.codeBug();
                // the new operator uses stack top to store the constructed
                // object so it shall not be cleared: see comments in
                // setCallResult
            }
            outermost = x;
            x = x.parentFrame;
        }
        
        if (requireContinuationsTopFrame) {
            while (outermost.parentFrame != null)
                outermost = outermost.parentFrame;
    
            if (!outermost.isContinuationsTopFrame) {
                throw new IllegalStateException("Cannot capture continuation " +
                        "from JavaScript code not called directly by " +
                        "executeScriptWithContinuations or " +
                        "callFunctionWithContinuations");
            }
        }
        
        c.initImplementation(frame);
        return c;
    }

    private static int stack_int32(CallFrame frame, int i)
    {
        Object x = frame.stack[i];
        double value;
        if (x == UniqueTag.DOUBLE_MARK) {
            value = frame.sDbl[i];
        } else {
            value = ScriptRuntime.toNumber(x);
        }
        return ScriptRuntime.toInt32(value);
    }

    private static double stack_double(CallFrame frame, int i)
    {
        Object x = frame.stack[i];
        if (x != UniqueTag.DOUBLE_MARK) {
            return ScriptRuntime.toNumber(x);
        } else {
            return frame.sDbl[i];
        }
    }

    private static boolean stack_boolean(CallFrame frame, int i)
    {
        Object x = frame.stack[i];
        if (x == Boolean.TRUE) {
            return true;
        } else if (x == Boolean.FALSE) {
            return false;
        } else if (x == UniqueTag.DOUBLE_MARK) {
            double d = frame.sDbl[i];
            return d == d && d != 0.0;
        } else if (x == null || x == Undefined.instance) {
            return false;
        } else if (x instanceof Number) {
            double d = ((Number)x).doubleValue();
            return (d == d && d != 0.0);
        } else if (x instanceof Boolean) {
            return ((Boolean)x).booleanValue();
        } else {
            return ScriptRuntime.toBoolean(x);
        }
    }

    private static void do_add(Object[] stack, double[] sDbl, int stackTop,
                              Context cx)
    {
        Object rhs = stack[stackTop + 1];
        Object lhs = stack[stackTop];
        double d;
        boolean leftRightOrder;
        if (rhs == UniqueTag.DOUBLE_MARK) {
            d = sDbl[stackTop + 1];
            if (lhs == UniqueTag.DOUBLE_MARK) {
                sDbl[stackTop] += d;
                return;
            }
            leftRightOrder = true;
            // fallthrough to object + number code
        } else if (lhs == UniqueTag.DOUBLE_MARK) {
            d = sDbl[stackTop];
            lhs = rhs;
            leftRightOrder = false;
            // fallthrough to object + number code
        } else {
            if (lhs instanceof Scriptable || rhs instanceof Scriptable) {
                stack[stackTop] = ScriptRuntime.add(lhs, rhs, cx);
            } else if (lhs instanceof String) {
                String lstr = (String)lhs;
                String rstr = ScriptRuntime.toString(rhs);
                stack[stackTop] = lstr.concat(rstr);
            } else if (rhs instanceof String) {
                String lstr = ScriptRuntime.toString(lhs);
                String rstr = (String)rhs;
                stack[stackTop] = lstr.concat(rstr);
            } else {
                double lDbl = (lhs instanceof Number)
                    ? ((Number)lhs).doubleValue() : ScriptRuntime.toNumber(lhs);
                double rDbl = (rhs instanceof Number)
                    ? ((Number)rhs).doubleValue() : ScriptRuntime.toNumber(rhs);
                stack[stackTop] = UniqueTag.DOUBLE_MARK;
                sDbl[stackTop] = lDbl + rDbl;
            }
            return;
        }

        // handle object(lhs) + number(d) code
        if (lhs instanceof Scriptable) {
            rhs = ScriptRuntime.wrapNumber(d);
            if (!leftRightOrder) {
                Object tmp = lhs;
                lhs = rhs;
                rhs = tmp;
            }
            stack[stackTop] = ScriptRuntime.add(lhs, rhs, cx);
        } else if (lhs instanceof String) {
            String lstr = (String)lhs;
            String rstr = ScriptRuntime.toString(d);
            if (leftRightOrder) {
                stack[stackTop] = lstr.concat(rstr);
            } else {
                stack[stackTop] = rstr.concat(lstr);
            }
        } else {
            double lDbl = (lhs instanceof Number)
                ? ((Number)lhs).doubleValue() : ScriptRuntime.toNumber(lhs);
            stack[stackTop] = UniqueTag.DOUBLE_MARK;
            sDbl[stackTop] = lDbl + d;
        }
    }

    private static Object[] getArgsArray(Object[] stack, double[] sDbl,
                                         int shift, int count)
    {
        if (count == 0) {
            return ScriptRuntime.emptyArgs;
        }
        Object[] args = new Object[count];
        for (int i = 0; i != count; ++i, ++shift) {
            Object val = stack[shift];
            if (val == UniqueTag.DOUBLE_MARK) {
                val = ScriptRuntime.wrapNumber(sDbl[shift]);
            }
            args[i] = val;
        }
        return args;
    }

    private static void addInstructionCount(Context cx, CallFrame frame,
                                            int extra)
    {
        cx.instructionCount += frame.pc - frame.pcPrevBranch + extra;
        if (cx.instructionCount > cx.instructionThreshold) {
            cx.observeInstructionCount(cx.instructionCount);
            cx.instructionCount = 0;
        }
    }
}
