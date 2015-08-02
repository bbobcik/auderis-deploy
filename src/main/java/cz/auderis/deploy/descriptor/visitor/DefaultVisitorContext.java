/*
 * Copyright 2015 Boleslav Bobcik - Auderis
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package cz.auderis.deploy.descriptor.visitor;

import cz.auderis.deploy.descriptor.bean.AbstractBean;

import java.io.Serializable;
import java.util.Collections;
import java.util.Deque;
import java.util.LinkedList;
import java.util.List;

public class DefaultVisitorContext implements VisitorContext, Serializable {
	private static final long serialVersionUID = -1406613205937411284L;

	final VisitOrder visitOrder;
	AbstractBean currentBean;
	final Deque<Object> context;
	final List<?> contextView;

	public DefaultVisitorContext() {
		this(VisitOrder.PARENT_LAST);
	}

	public DefaultVisitorContext(VisitOrder order) {
		this.visitOrder = (null != order) ? order : VisitOrder.PARENT_LAST;
		this.currentBean = null;
		final LinkedList<Object> contextList = new LinkedList<Object>();
		this.context = contextList;
		this.contextView = Collections.unmodifiableList(contextList);
	}

	@Override
	public VisitOrder getVisitOrder() {
		return visitOrder;
	}

	@Override
	public AbstractBean getCurrentBean() {
		return currentBean;
	}

	@Override
	public List<?> getContextParts() {
		return contextView;
	}

	@Override
	public Object getHeadContextPart() {
		if (context.isEmpty()) {
			return null;
		}
		return context.peek();
	}

	@Override
	public void pushContextPart(Object ctxPart) {
		if (null == ctxPart) {
			throw new NullPointerException();
		}
		context.push(ctxPart);
		if (ctxPart instanceof AbstractBean) {
			currentBean = (AbstractBean) ctxPart;
		}
	}

	@Override
	public void popContextPart() {
		final Object removedPart = context.pop();
		if (removedPart == currentBean) {
			currentBean = null;
		}
	}

}
