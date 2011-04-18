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

package org.mozilla.javascript;

import java.lang.reflect.Method;
import java.lang.reflect.Member;
import java.util.Iterator;

public abstract class VMBridge
{

    static final VMBridge instance = makeInstance();

    private static VMBridge makeInstance()
    {
        String[] classNames = {
            "org.mozilla.javascript.VMBridge_custom",
            "org.mozilla.javascript.jdk15.VMBridge_jdk15",
            "org.mozilla.javascript.jdk13.VMBridge_jdk13",
            "org.mozilla.javascript.jdk11.VMBridge_jdk11",
        };
        for (int i = 0; i != classNames.length; ++i) {
            String className = classNames[i];
            Class<?> cl = Kit.classOrNull(className);
            if (cl != null) {
                VMBridge bridge = (VMBridge)Kit.newInstanceOrNull(cl);
                if (bridge != null) {
                    return bridge;
                }
            }
        }
        throw new IllegalStateException("Failed to create VMBridge instance");
    }

    /**
     * Return a helper object to optimize {@link Context} access.
     * <p>
     * The runtime will pass the resulting helper object to the subsequent
     * calls to {@link #getContext(Object contextHelper)} and
     * {@link #setContext(Object contextHelper, Context cx)} methods.
     * In this way the implementation can use the helper to cache
     * information about current thread to make {@link Context} access faster.
     */
    protected abstract Object getThreadContextHelper();

    /**
     * Get {@link Context} instance associated with the current thread
     * or null if none.
     *
     * @param contextHelper The result of {@link #getThreadContextHelper()}
     *                      called from the current thread.
     */
    protected abstract Context getContext(Object contextHelper);

    /**
     * Associate {@link Context} instance with the current thread or remove
     * the current association if <tt>cx</tt> is null.
     *
     * @param contextHelper The result of {@link #getThreadContextHelper()}
     *                      called from the current thread.
     */
    protected abstract void setContext(Object contextHelper, Context cx);

    /**
     * Return the ClassLoader instance associated with the current thread.
     */
    protected abstract ClassLoader getCurrentThreadClassLoader();

    /**
     * In many JVMSs, public methods in private
     * classes are not accessible by default (Sun Bug #4071593).
     * VMBridge instance should try to workaround that via, for example,
     * calling method.setAccessible(true) when it is available.
     * The implementation is responsible to catch all possible exceptions
     * like SecurityException if the workaround is not available.
     *
     * @return true if it was possible to make method accessible
     *         or false otherwise.
     */
    protected abstract boolean tryToMakeAccessible(Object accessibleObject);

    /**
     * Create helper object to create later proxies implementing the specified
     * interfaces later. Under JDK 1.3 the implementation can look like:
     * <pre>
     * return java.lang.reflect.Proxy.getProxyClass(..., interfaces).
     *     getConstructor(new Class[] {
     *         java.lang.reflect.InvocationHandler.class });
     * </pre>
     *
     * @param interfaces Array with one or more interface class objects.
     */
    protected Object getInterfaceProxyHelper(ContextFactory cf,
                                             Class<?>[] interfaces)
    {
        throw Context.reportRuntimeError(
            "VMBridge.getInterfaceProxyHelper is not supported");
    }

    /**
     * Create proxy object for {@link InterfaceAdapter}. The proxy should call
     * {@link InterfaceAdapter#invoke(ContextFactory cf,
     *                                Object target,
     *                                Scriptable topScope,
     *                                Method method,
     *                                Object[] args)}
     * as implementation of interface methods associated with
     * <tt>proxyHelper</tt>.
     *
     * @param proxyHelper The result of the previous call to
     *        {@link #getInterfaceProxyHelper(ContextFactory, Class[])}.
     */
    protected Object newInterfaceProxy(Object proxyHelper,
                                       ContextFactory cf,
                                       InterfaceAdapter adapter,
                                       Object target,
                                       Scriptable topScope)
    {
        throw Context.reportRuntimeError(
            "VMBridge.newInterfaceProxy is not supported");
    }

    /**
     * Returns whether or not a given member (method or constructor)
     * has variable arguments.
     * Variable argument methods have only been supported in Java since
     * JDK 1.5.
     */
    protected abstract boolean isVarArgs(Member member);

    /**
     * If "obj" is a java.util.Iterator or a java.lang.Iterable, return a
     * wrapping as a JavaScript Iterator. Otherwise, return null.
     * This method is in VMBridge since Iterable is a JDK 1.5 addition.
     */
    public Iterator<?> getJavaIterator(Context cx, Scriptable scope, Object obj) {
        if (obj instanceof Wrapper) {
            Object unwrapped = ((Wrapper) obj).unwrap();
            Iterator<?> iterator = null;
            if (unwrapped instanceof Iterator)
                iterator = (Iterator<?>) unwrapped;
            return iterator;
        }
        return null;
    }
}
