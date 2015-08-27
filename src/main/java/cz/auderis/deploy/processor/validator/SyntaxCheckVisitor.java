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

package cz.auderis.deploy.processor.validator;

import cz.auderis.deploy.descriptor.bean.AbstractBean;
import cz.auderis.deploy.descriptor.bean.ExternalBundleBean;
import cz.auderis.deploy.descriptor.bean.NormalBean;
import cz.auderis.deploy.descriptor.bean.StandaloneListBean;
import cz.auderis.deploy.descriptor.bean.StandaloneMapBean;
import cz.auderis.deploy.descriptor.bean.StandaloneSetBean;
import cz.auderis.deploy.descriptor.dependency.BeanInjectionElement;
import cz.auderis.deploy.descriptor.dependency.ExtraDependency;
import cz.auderis.deploy.descriptor.dependency.PropertyInjectionElement;
import cz.auderis.deploy.descriptor.initializer.ConstructorArgumentElement;
import cz.auderis.deploy.descriptor.initializer.PropertyElement;
import cz.auderis.deploy.descriptor.visitor.DeploymentStructureVisitorAdapter;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;

import java.text.MessageFormat;
import java.util.ResourceBundle;
import java.util.regex.Matcher;

class SyntaxCheckVisitor extends DeploymentStructureVisitorAdapter {

	private final VisitorContext context;
	private final ResourceBundle validationBundle;

	SyntaxCheckVisitor(VisitorContext context) {
		this(context, null);
	}

	SyntaxCheckVisitor(VisitorContext context, ResourceBundle bundle) {
		assert null != context;
		this.context = context;
		if (null != bundle) {
			this.validationBundle = bundle;
		} else {
			this.validationBundle = ResourceBundle.getBundle("cz.auderis.deploy.processor.validator.Validator");
		}
	}

	@Override
	public void visitNormalBean(NormalBean normalBean) {
		visitTopLevelBean(normalBean);
		checkClassName(normalBean.getBeanClassName(), "class.badName.beanDefinition");
	}

	@Override
	public void visitListBean(StandaloneListBean listBean) {
		visitTopLevelBean(listBean);
		checkOptionalClassName(listBean.getItemClassName(), "class.badName.collectionItem");
	}

	@Override
	public void visitSetBean(StandaloneSetBean setBean) {
		visitTopLevelBean(setBean);
		checkOptionalClassName(setBean.getItemClassName(), "class.badName.collectionItem");
	}

	@Override
	public void visitMapBean(StandaloneMapBean mapBean) {
		visitTopLevelBean(mapBean);
		checkOptionalClassName(mapBean.getKeyClassName(), "class.badName.mapKey");
		checkOptionalClassName(mapBean.getValueClassName(), "class.badName.mapValue");
	}

	@Override
	public void visitBundleBean(ExternalBundleBean bundleBean) {
		visitTopLevelBean(bundleBean);
	}

	private void visitTopLevelBean(AbstractBean bean) {
		checkBeanName(bean.getName(), "bean.badName.beanDefinition");
	}

	@Override
	public void visitBeanPropertyDefinition(PropertyElement property) {
		checkPropertyName(property.getName(), "property.badName.propertyDefinition");
	}

	@Override
	public void visitBeanConstructorArgument(ConstructorArgumentElement constructorArgument) {
		checkPropertyName(constructorArgument.getPropertyName(), "property.badName.constructorArgument");
		final String className = constructorArgument.getArgumentClassName();
		if (null != className) {
			checkClassName(className, "class.badName.constructorArgument");
		}
	}

	@Override
	public void visitBeanInjection(BeanInjectionElement beanInjection) {
		if (!beanInjection.isAutowireInjection()) {
			checkBeanName(beanInjection.getBeanName(), "bean.badName.beanInjection");
		}
	}

	@Override
	public void visitPropertyInjection(PropertyInjectionElement propertyInjection) {
		checkBeanName(propertyInjection.getBeanName(), "bean.badName.propertyInjection");
		checkPropertyName(propertyInjection.getPropertyName(), "property.badName.propertyInjection");
	}

	@Override
	public void visitExtraDependency(ExtraDependency extraDependency) {
		checkBeanName(extraDependency.getBeanName(), "bean.badName.extraDependency");
	}

	@Override
	public void visitUnknownValue(Object item) {
		// TODO complain
	}

	private void checkBeanName(String beanName, String msgId) {
		if (null == beanName) {
			beanName = "";
		}
		final Matcher matcher = ValidationPattern.BEAN_NAME.matcher(beanName);
		if (matcher.matches()) {
			return;
		}
		//
		final AbstractBean currentBean = context.getCurrentBean();
		final String parentBeanName = currentBean.getName();
		final String msg = formatMessage(msgId, beanName, parentBeanName);
	}

	private void checkPropertyName(String propertyName, String msgId) {
		if (null == propertyName) {
			propertyName = "";
		}
		final Matcher matcher = ValidationPattern.PROPERTY_NAME.matcher(propertyName);
		if (matcher.matches()) {
			return;
		}
		//
		final AbstractBean currentBean = context.getCurrentBean();
		final String beanName = currentBean.getName();
		final String msg = formatMessage(msgId, propertyName, beanName);
	}

	private void checkClassName(String className, String msgId) {
		if (null == className) {
			className = "";
		}
		final Matcher matcher = ValidationPattern.JAVA_CLASS.matcher(className);
		if (matcher.matches()) {
			return;
		}
		//
		final AbstractBean currentBean = context.getCurrentBean();
		final String beanName = currentBean.getName();
		final String msg = formatMessage(msgId, className, beanName);
	}

	private void checkOptionalClassName(String className, String msgId) {
		if ((null == className) || className.trim().isEmpty()) {
			return;
		}
		checkClassName(className, msgId);
	}

	private String formatMessage(String msgId, Object... arguments) {
		final String template = validationBundle.getString(msgId);
		final String message = MessageFormat.format(template, arguments);
		return message;
	}

}
