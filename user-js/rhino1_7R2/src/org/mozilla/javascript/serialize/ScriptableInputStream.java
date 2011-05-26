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
 * The Original Code is Rhino serialization code, released
 * Sept. 25, 2001.
 *
 * The Initial Developer of the Original Code is
 * Norris Boyd.
 * Portions created by the Initial Developer are Copyright (C) 2001
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *   Norris Boyd
 *   Igor Bukanov
 *   Attila Szegedi
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

// API class

package org.mozilla.javascript.serialize;

import java.io.*;

import org.mozilla.javascript.*;

/**
 * Class ScriptableInputStream is used to read in a JavaScript
 * object or function previously serialized with a ScriptableOutputStream.
 * References to names in the exclusion list
 * replaced with references to the top-level scope specified during
 * creation of the ScriptableInputStream.
 *
 * @author Norris Boyd
 */

public class ScriptableInputStream extends ObjectInputStream {

    /**
     * Create a ScriptableInputStream.
     * @param in the InputStream to read from.
     * @param scope the top-level scope to create the object in.
     */
    public ScriptableInputStream(InputStream in, Scriptable scope)
        throws IOException
    {
        super(in);
        this.scope = scope;
        enableResolveObject(true);
        Context cx = Context.getCurrentContext();
        if (cx != null) {
            this.classLoader = cx.getApplicationClassLoader();
        }
    }

    @Override
    protected Class<?> resolveClass(ObjectStreamClass desc)
        throws IOException, ClassNotFoundException
    {
        String name = desc.getName();
        if (classLoader != null) {
            try {
                return classLoader.loadClass(name);
            } catch (ClassNotFoundException ex) {
                // fall through to default loading
            }
        }
        return super.resolveClass(desc);
    }

    @Override
    protected Object resolveObject(Object obj)
        throws IOException
    {
        if (obj instanceof ScriptableOutputStream.PendingLookup) {
            String name = ((ScriptableOutputStream.PendingLookup)obj).getName();
            obj = ScriptableOutputStream.lookupQualifiedName(scope, name);
            if (obj == Scriptable.NOT_FOUND) {
                throw new IOException("Object " + name + " not found upon " +
                                      "deserialization.");
            }
        }else if (obj instanceof UniqueTag) {
            obj = ((UniqueTag)obj).readResolve();
        }else if (obj instanceof Undefined) {
            obj = ((Undefined)obj).readResolve();
        }
        return obj;
    }

    private Scriptable scope;
    private ClassLoader classLoader;
}
