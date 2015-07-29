package cz.auderis.deploy.descriptor.initializer;

import javax.xml.bind.annotation.XmlElementRef;
import javax.xml.bind.annotation.XmlType;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@XmlType(name = "constructor")
public class ConstructorElement implements Serializable {
	private static final long serialVersionUID = 20150728L;

	@XmlElementRef
	protected List<ConstructorArgumentElement> arguments;

	public ConstructorElement() {
		super();
		this.arguments = new ArrayList<ConstructorArgumentElement>(1);
	}

	public List<ConstructorArgumentElement> getArguments() {
		return arguments;
	}

}
