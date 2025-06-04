package com.unper.samper.model;

import java.util.HashSet;
import java.util.Set;

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.unper.samper.model.common.Audit;
import com.unper.samper.model.constant.EPrevillage;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
@Entity
@Table(name = "privillage", schema = "portal")
public class Privilage extends Audit {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer id;

    @ManyToOne(fetch = FetchType.LAZY, targetEntity = BasePrivilage.class)
    @JoinColumn(name = "base_previllage_id")
    private BasePrivilage basePrevillage;

    private String url;

    @Enumerated(EnumType.STRING)
    private EPrevillage previllage;

    @Builder.Default
    @ManyToMany(mappedBy = "previllages")
    private Set<Admin> admins = new HashSet<>();
}
