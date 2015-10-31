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

package cz.auderis.deploy.parser;

import cz.auderis.deploy.LogName;
import cz.auderis.deploy.descriptor.bean.AbstractBean;
import cz.auderis.deploy.descriptor.bean.BeanConflictMode;
import cz.auderis.deploy.descriptor.bean.BeanLifecycleStage;
import cz.auderis.deploy.descriptor.bean.BeanType;
import cz.auderis.deploy.descriptor.bean.ExternalBundleBean;
import cz.auderis.deploy.descriptor.bean.NormalBean;
import cz.auderis.deploy.descriptor.bean.StandaloneListBean;
import cz.auderis.deploy.descriptor.bean.StandaloneMapBean;
import cz.auderis.deploy.descriptor.bean.StandaloneSetBean;
import cz.auderis.deploy.descriptor.visitor.DeploymentVisitorAdapter;
import cz.auderis.deploy.descriptor.visitor.VisitorContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.text.MessageFormat;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.Set;

public class BeanLocatorVisitor extends DeploymentVisitorAdapter {

	public static final String DEFAULT_RESOURCE_BUNDLE = "cz.auderis.deploy.parser.BeanLocator";
	private static final Logger LOG = LoggerFactory.getLogger(LogName.PARSER);

	private final VisitorContext context;
	private final Map<String, AbstractBean> beanMap;
	private final ResourceBundle resourceBundle;

	public BeanLocatorVisitor(VisitorContext context) {
		this(context, null);
	}

	public BeanLocatorVisitor(VisitorContext context, ResourceBundle bundle) {
		if (null == context) {
			throw new NullPointerException();
		}
		this.context = context;
		this.beanMap = new LinkedHashMap<String, AbstractBean>(32);
		if (null != bundle) {
			this.resourceBundle = bundle;
		} else {
			this.resourceBundle = ResourceBundle.getBundle(DEFAULT_RESOURCE_BUNDLE);
		}
	}

	@Override
	public void visitNormalBean(NormalBean normalBean) {
		doCommonBeanProcessing(normalBean);
	}

	@Override
	public void visitListBean(StandaloneListBean listBean) {
		doCommonBeanProcessing(listBean);
	}

	@Override
	public void visitSetBean(StandaloneSetBean setBean) {
		doCommonBeanProcessing(setBean);
	}

	@Override
	public void visitMapBean(StandaloneMapBean mapBean) {
		doCommonBeanProcessing(mapBean);
	}

	@Override
	public void visitBundleBean(ExternalBundleBean bundleBean) {
		doCommonBeanProcessing(bundleBean);
	}

	private boolean doCommonBeanProcessing(AbstractBean bean) {
		final String name = bean.getName();
		if (!beanMap.containsKey(name)) {
			beanMap.put(name, bean);
			LOG.trace("Registered bean {}", bean);
			return true;
		}
		final AbstractBean prevBean = beanMap.get(name);
		// Check bean type compatibility
		final BeanType type  = bean.getBeanType();
		if (prevBean.getBeanType() != type) {
			LOG.debug("Type conflict of bean {} with new bean {}", prevBean, bean);
			throw new BeanDefinitionException(formatMessage("bean.conflict.type", name, type));
		}
		// Apply conflict mode
		final BeanConflictMode conflictMode = bean.getConflictResolutionMode();
		final BeanConflictMode prevMode = prevBean.getConflictResolutionMode();
		final Set<BeanConflictMode> conflictUnion = EnumSet.of(conflictMode, prevMode);
		if (conflictUnion.contains(BeanConflictMode.FAIL)) {
			LOG.debug("Override of bean {} by bean {} forbidden", prevBean, bean);
			prevBean.setLifecycleStage(BeanLifecycleStage.EAGER_CONFLICT_FAILURE);
			throw new BeanDefinitionException(formatMessage("bean.conflict.forbidden", name));
		} else if (conflictUnion.contains(BeanConflictMode.LAZY_FAIL)) {
			LOG.debug("Bean {} set for instantiation failure due to existence of bean {}", prevBean, bean);
			prevBean.setLifecycleStage(BeanLifecycleStage.LAZY_CONFLICT_FAILURE);
			return true;
		}
		switch (prevMode) {
			case IGNORE:
				LOG.debug("Bean {} ignored, definition of bean {} kept", bean, prevBean);
				break;
			case REPLACE:
				LOG.debug("Bean {} replaces bean {}", bean, prevBean);
				beanMap.put(name, bean);
				break;
			case UPDATE:
				LOG.debug("Bean {} is updated by definition of bean {}", prevBean, bean);
				prevBean.updateDefinition(bean);
				return false;
			default:
				throw new AssertionError("Mode " + prevMode + " not handled");
		}
		return true;
	}

	private String formatMessage(String msgId, Object... arguments) {
		final String template = resourceBundle.getString(msgId);
		final String message = MessageFormat.format(template, arguments);
		return message;
	}

}
