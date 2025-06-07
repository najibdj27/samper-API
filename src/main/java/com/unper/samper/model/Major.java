package com.unper.samper.model;

import java.util.HashSet;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import org.springframework.boot.autoconfigure.EnableAutoConfiguration;

import com.unper.samper.model.common.Audit;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@EnableAutoConfiguration
@Getter
@Setter
@Builder
@Entity
@Table(name = "major", schema = "common", uniqueConstraints = {@UniqueConstraint(columnNames = "majorCode")})
public class Major extends Audit {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String majorCode;

    private String name;

    @OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY, targetEntity = Lecture.class)
    @JoinColumn(name = "major_head")
    private Lecture majorHead;

    @Builder.Default
    @ManyToMany(mappedBy = "majors")
    private Set<Subject> subjects = new HashSet<>();

    @Builder.Default
    private Boolean isDeleted = Boolean.FALSE;
} 
