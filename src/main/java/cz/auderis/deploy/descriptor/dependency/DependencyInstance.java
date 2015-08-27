package cz.auderis.deploy.descriptor.dependency;

import cz.auderis.deploy.descriptor.initializer.InjectionPlace;

import java.io.Serializable;
import java.util.Objects;

public class DependencyInstance implements Serializable {
	private static final long serialVersionUID = 20150821L;

	final InjectionPlace place;
	final InjectionType type;
	final MaterializationMode materializationMode;
	final boolean hardLink;
	final String propertyName;

	public static Builder builder() {
		return new BuilderImpl();
	}

	DependencyInstance(InjectionPlace place, InjectionType type, MaterializationMode matMode, boolean hardLink, String propertyName) {
		this.place = place;
		this.type = type;
		this.materializationMode = matMode;
		this.hardLink = hardLink;
		this.propertyName = propertyName;
	}

	public InjectionPlace getPlace() {
		return place;
	}

	public InjectionType getType() {
		return type;
	}

	public MaterializationMode getMaterializationMode() {
		return materializationMode;
	}

	public boolean isHardLink() {
		return hardLink;
	}

	public String getPropertyName() {
		return propertyName;
	}

	@Override
	public int hashCode() {
		return Objects.hash(place, propertyName, type);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		} else if (!(obj instanceof DependencyInstance)) {
			return false;
		}
		final DependencyInstance other = (DependencyInstance) obj;
		if (this.hardLink != other.isHardLink()) {
			return false;
		} else if (type != other.getType()) {
			return false;
		} else if (this.materializationMode != other.getMaterializationMode()) {
			return false;
		} else if (!Objects.equals(this.place, other.getPlace())) {
			return false;
		} else if (!Objects.equals(this.propertyName, other.getPropertyName())) {
			return false;
		}
		return true;
	}


	public interface Builder {
		DependencyInstance build();

		InjectionPlace getPlace();

		void setPlace(InjectionPlace place);

		InjectionType getType();

		void setType(InjectionType type);

		MaterializationMode getMaterializationMode();

		void setMaterializationMode(MaterializationMode materializationMode);

		boolean isHardLink();

		void setHardLink(boolean hardLink);

		String getPropertyName();

		void setPropertyName(String propertyName);
	}

	static final class BuilderImpl implements Builder {
		InjectionPlace place;
		InjectionType type;
		MaterializationMode materializationMode;
		boolean hardLink;
		String propertyName;

		BuilderImpl() {
			type = InjectionType.BEAN;
			materializationMode = MaterializationMode.FINAL_BEAN;
			hardLink = true;
		}

		@Override
		public DependencyInstance build() {
			if (null == place) {
				throw new IllegalStateException("Injection place not defined");
			} else if ((null == propertyName) && (InjectionType.PROPERTY == type)) {
				throw new IllegalStateException("Injected bean property not defined");
			}
			final DependencyInstance instance = new DependencyInstance(place, type, materializationMode, hardLink, propertyName);
			return instance;
		}

		@Override
		public InjectionPlace getPlace() {
			return place;
		}

		@Override
		public void setPlace(InjectionPlace place) {
			this.place = place;
		}

		@Override
		public InjectionType getType() {
			return type;
		}

		@Override
		public void setType(InjectionType type) {
			if (null == type) {
				throw new NullPointerException();
			}
			this.type = type;
		}

		@Override
		public MaterializationMode getMaterializationMode() {
			return materializationMode;
		}

		@Override
		public void setMaterializationMode(MaterializationMode materializationMode) {
			if (null == materializationMode) {
				throw new NullPointerException();
			}
			this.materializationMode = materializationMode;
		}

		@Override
		public boolean isHardLink() {
			return hardLink;
		}

		@Override
		public void setHardLink(boolean hardLink) {
			this.hardLink = hardLink;
		}

		@Override
		public String getPropertyName() {
			return propertyName;
		}

		@Override
		public void setPropertyName(String propertyName) {
			this.propertyName = propertyName;
		}
	}

}
