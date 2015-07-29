package cz.auderis.deploy.descriptor;


import cz.auderis.deploy.descriptor.bean.NormalBean;
import cz.auderis.deploy.descriptor.dependency.BeanInjectionElement;
import cz.auderis.deploy.descriptor.initializer.PropertyElement;
import cz.auderis.deploy.descriptor.producer.FactoryDefinition;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public final class DescriptorParserSupport {

	private static final List<Class<?>> JAXB_PACKAGE_REPRESENTING_CLASSES = getRepresentingClasses();

	public static String getJaxbContextPackages() {
		final StringBuilder str = new StringBuilder(512);
		for (final Class<?> repClass : JAXB_PACKAGE_REPRESENTING_CLASSES) {
			final Package pkg = repClass.getPackage();
			final String pkgName = pkg.getName();
			if (0 != str.length()) {
				str.append(':');
			}
			str.append(pkgName);
		}
		return str.toString();
	}

	public static boolean isStrictParsingEnabled() {
		return !Boolean.getBoolean("auderis.deploy.lenient");
	}

	public static boolean isBlank(String arg) {
		if (null == arg) {
			return true;
		}
		final int len = arg.length();
		for (int i=0; i<len; ++i) {
			final char c = arg.charAt(i);
			if (!Character.isWhitespace(c)) {
				return false;
			}
		}
		return true;
	}

	public static Set<String> recognizedNameSet(String canonicalName, String ... aliases) {
		final Set<String> allNames = new HashSet<String>(1 + aliases.length);
		allNames.add(canonicalName);
		allNames.addAll(Arrays.asList(aliases));
		assert !allNames.contains(null);
		assert !allNames.contains("");
		return Collections.unmodifiableSet(allNames);
	}

	public static <T extends Enum<T> & NamedEnum> T parseEnumByName(String name, Class<T> enumClass) {
		if (isBlank(name)) {
			return null;
		}
		final T[] enumConstants = enumClass.getEnumConstants();
		if (isStrictParsingEnabled()) {
			for (final T enumConstant : enumConstants) {
				if (enumConstant.getCanonicalName().equals(name)) {
					return enumConstant;
				}
			}
		} else {
			final String normalizedName = name.trim().toLowerCase();
			for (final T enumConstant : enumConstants) {
				for (final String enumName : enumConstant.getRecognizedNames()) {
					if (normalizedName.equalsIgnoreCase(enumName)) {
						return enumConstant;
					}
				}
			}
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	private static List<Class<?>> getRepresentingClasses() {
		final List<Class<?>> result = Arrays.asList(
				(Class<?>) Deployment.class,
				(Class<?>) NormalBean.class,
				(Class<?>) BeanInjectionElement.class,
				(Class<?>) PropertyElement.class,
				(Class<?>) FactoryDefinition.class
		);
		return Collections.unmodifiableList(result);
	}

	private DescriptorParserSupport() {
		throw new AssertionError();
	}

	public interface NamedEnum {
		String getCanonicalName();
		Set<String> getRecognizedNames();
	}

}
